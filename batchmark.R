# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# See also attic/_dependencies.R
# renv::restore(prompt = FALSE)
rs = renv::status()
if (!rs$synchronized) {
  cli::cli_alert_danger("renv library not synchronized!")
}

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("mlr3mbo")
library("batchtools", warn.conflicts = FALSE)
library("mlr3batchmark")
# renv::install("mlr-org/mlr3extralearners@surv_updates")
requireNamespace("mlr3extralearners")

# Create Registry ---------------------------------------------------------

if (!fs::dir_exists(fs::path_dir(conf$reg_dir))) {
  fs::dir_create(fs::path_dir(conf$reg_dir))
}

if (fs::dir_exists(conf$reg_dir)) {
  if (config::is_active("production")) {
    cli::cli_abort("Refusing to delete existing registry {.file {fs::path_rel(conf$reg_dir)}} in production mode!")
  } else {
    cli::cli_alert_warning("Deleting registry at {.file {fs::path_rel(conf$reg_dir)}}")
    fs::dir_delete(conf$reg_dir)
  }
}

cli::cli_alert_info("Creating new registry {.val {conf$reg_name}}!")
reg = makeExperimentRegistry(
  conf$reg_dir,
  work.dir = here::here(),
  seed = conf$seed,
  packages = c("mlr3", "mlr3proba"),
  source = here::here("helpers.R")
)

# Tasks ---------------------------------------------------------------------------------------
source(here::here("tasks.R"))

# Base learner setup ------------------------------------------------------
#' Base learner with fallback + encapsulation, preprocessing pipeline and composition
#' @param key Learner key passed to `lrn()`.
#' @param ... Additional arguments passed to `lrn()`.
#' @param .encode Use `po("encode", method = "treatment")`? Set `TRUE` for e.g. XGBoost.
#' @param .scale Use `po("scale")`? Set `TRUE` for e.g. SSVM.
bl = function(key, ..., .encode = FALSE, .scale = FALSE) {
  cli::cli_h2("Constructing {.val {key}}")

  # 1. fixfactors ensures factor levels are the same during train and predict
  # - might introduce missings, hence
  # 2. imputesample to impute them, analogously to robustify pipeline
  # 3. collapsefactors reduces the number of factor levels
  # notable cases: hdfail, bladder0, whas, aids.id with a) many b) rare factor levels
  # Proposed change LB:
  # - Switch to no_collapse_above_prevalence = 0.05 to collapse levels up to 5% prevalence
  # - Keep target_level_count = 5 to not reduce to fewer than 5 levels total
  # (see also attic/data_check_factors.Rmd)
  preproc = po("fixfactors") %>>%
    po("imputesample", affect_columns = selector_type("factor")) %>>%
    po("collapsefactors",
       no_collapse_above_prevalence = 0.05,
       target_level_count = 5)

  # scaling only used for SSVM if DL removed
  # Done before treatment encoding
  if (.scale) {
    cli::cli_alert_info("Applying scaling")
    preproc = preproc %>>%
      po("scale")
  }

  # treatment encoding only for selected learners that don't support handling factors
  # Note: encoding is done for glmnet but not for coxph. Both are internally a
  # cox model, but glmnet does not do the treatment encoding automatically
  if (.encode) {
    cli::cli_alert_info("Applying factor encoding")
    preproc = preproc %>>%
      po("encode", method = "treatment")
  }

  # removeconstants: should constant features be introduced, they're dropped.
  #  - Done after treatment encoding
  # Stack preprocessing on top of learner + distr stuff.
  graph_learner = preproc %>>%
    po("removeconstants") %>>%
    lrn(key, ...) |>
    as_learner()

  graph_learner$predict_type = "crank"
  if (conf$fallback$inner) {
    suppressWarnings(graph_learner$encapsulate("evaluate", lrn("surv.kaplan")))
  } else {
    cli::cli_alert_info("Not applying fallback learner for inner GraphLearner")
  }

  # Edge case for timeout: CoxBoost ("surv.cv_coxboost") has it's own tuning mechanism, so
  # it's not wrapped in autotuner (like KM, NA). Timeout should therefore be set to
  # "outer" timeout, same as autotuners.
  # Choosing not to do this for KM and NA as they are fast enough to not cause issues here.
  if (key == "surv.cv_coxboost") {
    cli::cli_alert_info("Applying timeout for inner GraphLearner - CoxBoost exception!")
    graph_learner$timeout = c(
      train   = conf$timeout$at_train   * 3600,
      predict = conf$timeout$at_predict * 3600
    )
  } else {
    graph_learner$timeout = c(
      train   = conf$timeout$bl_train   * 3600,
      predict = conf$timeout$bl_predict * 3600
    )
  }

  # Used for XGBoost learners to enable internal tuning / early stopping using test set
  if ("validation" %in% graph_learner$properties) {
    cli::cli_alert_info("Setting validation field to {.val test}")
    set_validate(graph_learner, "test")
    checkmate::assert_true(graph_learner$validate == "test")
  }

  graph_learner
}

# AutoTuner -----------------------------------------------------------------------------------

#' Takes a base GraphLearner and search space and wraps it into AutoTuner
#' @param learner GRaphLearner returned by `bl()`.
#' @param ... Parameter search space, passed as `ps(...)`.
#' @param use_grid_search (`FALSE`) FOr learners with small categorical search spaces, it's more
#'   efficient to use grid search rather than to random search over e.g. 10 elements with 50 iters.
wrap_auto_tune = function(learner, ..., use_grid_search = FALSE) {
  learner = as_learner(learner)
  search_space = ps(...)
  if (is.null(search_space$trafo))
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))

  cli::cli_alert_warning("Using {.val {conf$tuning$resampling}} inner resampling!")

  resampling = switch(
    conf$tuning$resampling,
    "cv"          = rsmp("cv", folds = conf$tuning$folds),
    "repeated_cv" = rsmp("repeated_cv", folds = conf$tuning$folds, repeats = conf$tuning$repeats),
    "holdout"     = rsmp("holdout", ratio = conf$tuning$ratio)
  )


  # Need to switch tuner/trm since some learners have very small search spaces
  # Here, we use grid search to efficiently search the limited (fewer than 50 unique HPCs)
  # search space and not waste compute by repeatedly evaluating the same HPCs

  # run_time: maximum time tuning is allowed to run, seconds (evaluated after all inner resampling iters)
  trm_runtime = trm("run_time", secs = conf$budget$runtime_hours * 60 * 60)
  # evals: budget set in conf.R: n_evals + k * dim_search_space
  trm_evals = trm("evals",
                  n_evals = conf$budget$evals_constant,
                  k = conf$budget$evals_multiplier)

  if (use_grid_search) {
    # Use resolution that is normally greater than number of unique HPCs
    # For factors etc. this is fine, and for RRT (integer, 46 vals) this is also fine.
    # Also allows shorter runs during testing with small budget
    # Goal is to use grid_search with run_time terminator only

    # Account for pretest where we want 1 eval, so budget_constant may be 1
    # and multiplier 0
    if (conf$budget$evals_constant + conf$budget$evals_multiplier == 1) {
      grid_resolution = 1
    } else if (conf$budget$evals_multiplier == 0) {
      grid_resolution = conf$budget$evals_constant
    } else {
      grid_resolution = conf$budget$evals_multiplier
    }

    cli::cli_alert_info("Using grid search for tuning with resolution {.val {grid_resolution}}")
    tuner = tnr("grid_search", resolution = grid_resolution)
    terminator = trm_runtime
  } else {
    # combo terminator https://bbotk.mlr-org.com/reference/mlr_terminators_combo.html
    terminator = trm("combo", list(trm_runtime, trm_evals), any = TRUE)
    tuner = tnr("mbo")
  }

  callback_backup = callback_batch_tuning(
    id = "mlr3tuning.backup_archive",
    label = "Store tunign archives as RDS in registry",
    on_optimization_end = callback_backup_impl
  )

  # Check learner_id, used for disambiguation of individually saved tuning archives
  known_learners = c("surv.kaplan", "surv.nelson", "surv.akritas", "surv.coxph",
                     "surv.cv_glmnet", "surv.penalized", "surv.parametric", "surv.flexible",
                     "surv.rfsrc", "surv.ranger", "surv.cforest", "surv.aorsf", "surv.rpart",
                     "surv.mboost", "surv.cv_coxboost", "surv.xgboost.cox", "surv.xgboost.aft",
                     "surv.svm")
  pattern = paste0(known_learners, collapse = "|")
  learner_id = stringr::str_extract(learner$id, pattern)
  checkmate::assert_string(learner_id, min.chars = 7, pattern = "^surv")

  callback_backup$state$path_dir = fs::path(conf$reg_dir, "tuning_archives")
  callback_backup$state$learner_id = learner_id
  callback_backup$state$tuning_measure = measure$id

  callback_archive_logs = callback_batch_tuning(
    id = "mlr3tuning.archive_logs",
    label = "Add tuning logs to archive to detect fallback usage",
    on_eval_before_archive = callback_archive_logs_impl
  )

  at = auto_tuner(
    learner = learner,
    search_space = search_space,
    resampling = resampling,
    # Measure will be set via global variable in loop
    measure = measure,
    terminator = terminator,
    tuner = tuner,
    # Need tuning instance for archive, need archive to know if fallback was needed
    # Callback writes out archive, no need to store instance explicitly
    store_tuning_instance = conf$store$tuning_instance,
    # Not needed: benchmark result of inner resamplings
    store_benchmark_result = conf$store$benchmark_result,
    # Don't need models, only needed for variable imp etc. afaict
    store_models = conf$store$models,
    callbacks = list(callback_backup, callback_archive_logs)
  )

  # Ensure AutoTuner also has encapsulation and fallback in case of errors during outer resampling
  # which would not be caught by fallback/encaps during inner resampling with GraphLearner
  if (conf$fallback$outer) {
    suppressWarnings(at$encapsulate("callr", lrn("surv.kaplan")))
  } else {
    cli::cli_alert_info("Not applying fallback for outer AutoTuner!")
  }

  # Timeouts provided in hours via conf, converted to seconds.
  # Ensures computational job can finish prematurely given cluster timeout would kill it otherwise.
  at$timeout = c(
    train = conf$timeout$at_train * 3600,
    predict = conf$timeout$at_predict * 3600
  )

  at
}

# Set tuning measures -----------------------------------------------------
measures = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.brier", id = "isbs", p_max = 0.8, proper = FALSE, ERV = FALSE)
)

# Assemble learners -------------------------------------------------------
for (measure in measures) {

  cli::cli_h1("Assembling learners for {.val {measure$id}}")

  learners = list(
    KM = bl("surv.kaplan")

    ,

    `NA` = bl("surv.nelson")

    ,

    # survivalmodels::akritas
    # https://raphaels1.github.io/survivalmodels/reference/akritas.html
    AK = wrap_auto_tune(
      bl("surv.akritas"),
      surv.akritas.lambda = p_dbl(0, 1)
    )

    ,

    CPH = bl("surv.coxph")

    ,

    GLMN = wrap_auto_tune(
      bl("surv.cv_glmnet", .encode = TRUE),
      surv.cv_glmnet.alpha = p_dbl(0, 1)
    )

    ,

    Pen = wrap_auto_tune(
      bl("surv.penalized"),
      surv.penalized.lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
      surv.penalized.lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
    )

    ,

    # Use grid search due to small + finite search space
    # AFT version needs
    # - to pass .form to bl() for distrcompositor
    # - Tune distributions within range of what's sensible/discussed with RS
    Par = wrap_auto_tune(
      bl("surv.parametric", form = "aft", discrete = TRUE),
      surv.parametric.dist = p_fct(c("weibull", "exponential", "lognormal",  "loglogistic")),
      use_grid_search = TRUE
    )

    ,

    # Use grid search due to small + finite search space
    Flex = wrap_auto_tune(
      bl("surv.flexible"),
      surv.flexible.k = p_int(1, 10),
      use_grid_search = TRUE
    )

    ,

    RFSRC = wrap_auto_tune(
      # Fixing ntime = 150 (current default) just to be explicit, as ranger's time.interest
      # is set to a non-default value and we ensure both use 150 time points for evaluation
      bl("surv.rfsrc", ntree = 1000, ntime = 150),
      surv.rfsrc.splitrule = p_fct(c("bs.gradient", "logrank")),
      surv.rfsrc.mtry.ratio = p_dbl(0, 1),
      surv.rfsrc.nodesize = p_int(1, 50),
      surv.rfsrc.samptype = p_fct(c("swr", "swor")),
      surv.rfsrc.sampsize.ratio = p_dbl(0, 1)
    )

    ,

    RAN = wrap_auto_tune(
      # Adjusting time.interest (new as of 0.16.0) to 150, same as current RFSRC default
      bl("surv.ranger", num.trees = 1000, time.interest = 150),
      surv.ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
      surv.ranger.mtry.ratio = p_dbl(0, 1),
      surv.ranger.min.node.size = p_int(1, 50),
      surv.ranger.replace = p_lgl(),
      surv.ranger.sample.fraction = p_dbl(0, 1)
    )

    ,

    CIF = wrap_auto_tune(
      bl("surv.cforest", ntree = 1000),
      surv.cforest.mtryratio = p_dbl(0, 1),
      surv.cforest.minsplit = p_int(1, 50),
      surv.cforest.mincriterion = p_dbl(0, 1),
      surv.cforest.replace = p_lgl(),
      surv.cforest.fraction = p_dbl(0, 1)
    )

    ,

    ORSF = wrap_auto_tune(
      bl("surv.aorsf", n_tree = 1000, control_type = "fast"),
      surv.aorsf.mtry_ratio = p_dbl(0, 1),
      surv.aorsf.leaf_min_events = p_int(5, 50),
      .extra_trafo = function(x, param_set) {
        x$surv.aorsf.split_min_obs = x$surv.aorsf.leaf_min_events + 5L
        x
      }
    )

    ,

    RRT = wrap_auto_tune(
      bl("surv.rpart"),
      surv.rpart.minbucket = p_int(5, 50),
      use_grid_search = TRUE
    )

    ,

    MBST = wrap_auto_tune(
      bl("surv.mboost"),
      surv.mboost.family = p_fct(c("gehan", "cindex", "coxph", "weibull")),
      surv.mboost.mstop = p_int(10, 5000),
      surv.mboost.nu = p_dbl(0, 0.1),
      surv.mboost.baselearner = p_fct(c("bols", "btree"))
    )

    ,

    # Does not use our inner resampling
    CoxB = bl("surv.cv_coxboost",
              penalty = "optimCoxBoostPenalty",
              maxstepno = 5000,
              # Number of inner tuning folds: analogous to other AutoTuners
              K = conf$tuning$folds,
              .encode = TRUE)

    ,

    # XGB/cox, uses breslow estimator internally via mlr3proba
    XGBCox = wrap_auto_tune(
      bl("surv.xgboost.cox", tree_method = "hist", booster = "gbtree",
         early_stopping_rounds = 50,
         .encode = TRUE),
      surv.xgboost.cox.nrounds = p_int(
        upper = 5000,
        tags = "internal_tuning",
        aggr = function(x) as.integer(mean(unlist(x)))
      ),
      surv.xgboost.cox.max_depth = p_int(1, 20),
      surv.xgboost.cox.subsample = p_dbl(0, 1),
      surv.xgboost.cox.colsample_bytree = p_dbl(0, 1),
      surv.xgboost.cox.eta = p_dbl(0, 1),
      surv.xgboost.cox.grow_policy = p_fct(c("depthwise", "lossguide"))
    )

    ,

    # AFT version
    # - Tune distributions (as per JZ)
    XGBAFT = wrap_auto_tune(
      bl("surv.xgboost.aft", tree_method = "hist", booster = "gbtree",
         early_stopping_rounds = 50,
         .encode = TRUE),
      surv.xgboost.aft.nrounds = p_int(
        upper = 5000,
        tags = "internal_tuning",
        aggr = function(x) as.integer(mean(unlist(x)))
      ),
      surv.xgboost.aft.max_depth = p_int(1, 20),
      surv.xgboost.aft.subsample = p_dbl(0, 1),
      surv.xgboost.aft.colsample_bytree = p_dbl(0, 1),
      surv.xgboost.aft.eta = p_dbl(0, 1),
      surv.xgboost.aft.grow_policy = p_fct(c("depthwise", "lossguide")),
      surv.xgboost.aft.aft_loss_distribution = p_fct(c("normal", "logistic", "extreme")),
      surv.xgboost.aft.aft_loss_distribution_scale = p_dbl(0.5, 2.0)
    )

    ,

    SSVM = wrap_auto_tune(
      bl("surv.svm", type = "hybrid",
         diff.meth = "makediff3",
         # Set initial values but unused due to tuning
         gamma = 1, mu = 0,
         .encode = TRUE, .scale = TRUE),
      surv.svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
      surv.svm.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x)#,
      # Trafo no longer need when https://github.com/mlr-org/mlr3extralearners/pull/385
      # is merged
      # .extra_trafo = function(x, param_set) {
      #   # learner has tuple param gamma.mu = c(x, y)
      #   # we tune separately and reassemble via trafo
      #   x$surv.svm.gamma.mu = c(x$surv.svm.gamma, x$surv.svm.mu)
      #   x$surv.svm.gamma = x$surv.svm.mu = NULL
      #   x
      # }
    )

  )

  imap(learners, function(l, id) l$id = id)

  cli::cli_h2("Cleaning up and adding to registry")

  if (measure$id == "isbs") {
    cli::cli_alert_warning("Skipping {.val RRT} for ISBS measure!")
    learners$RRT = NULL

    cli::cli_alert_warning("Skipping {.val XGBAFT} for ISBS measure!")
    learners$XGBAFT = NULL

    cli::cli_alert_warning("Skipping {.val SSVM} for ISBS measure!")
    learners$SSVM = NULL
  }

  # custom grid design (with instantiated resamplings)
  grid = cross_join(list(task = tasks, learner = learners), sorted = FALSE)
  grid$resampling = rep(resamplings, each = length(learners))
  # If we want to keep the AutoTuner's tuning instance we have to set store_models
  # here, which counter intuitively is different from the `store_models` option in auto_tuner()
  # Similarly we also set it to TRUE if we want to store the models in general
  ids = batchmark(
    design =  grid,
    store_models = conf$store$tuning_instance | conf$store$models
  )
  # Tagging with the measure is used to disambiguate jobs with identical learner/task but different
  # tuning measure. Not sure if "cleaner" solution available?
  addJobTags(ids, measure$id)

  # also tag jobs which have been skipped because they are not wrapped
  # into a AutoTuner (and as such don't differ)
  learners_skipped = ids(learners)[!map_lgl(learners, inherits, "AutoTuner")]
  ids = findExperiments(algo.pars = learner_id %in% learners_skipped)
  addJobTags(ids, measure$id)
}

experiments = summarizeExperiments(by = c("task_id", "learner_id"))

cli::cli_alert_success("Added {.val {sum(experiments$.count)}} experiments to registry {.val {conf$reg_name}}")

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
  source = here::here(c("R/helpers.R", "R/learner-wrappers.R"))
)

# Tasks ---------------------------------------------------------------------------------------
source(here::here("tasks.R"))
source(here::here("R/learner-wrappers.R"))

# Set tuning measures -----------------------------------------------------
measures = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.brier", id = "isbs", p_max = 0.8, proper = FALSE, ERV = FALSE)
)

# Assemble learners -------------------------------------------------------
for (measure in measures) {
  cli::cli_h1("Assembling learners for {.val {measure$id}}")

  learners = list(
    KM = bl("surv.kaplan", id = "kaplan"),

    NEL = bl("surv.nelson", id = "nelson"),

    # survivalmodels::akritas
    # https://raphaels1.github.io/survivalmodels/reference/akritas.html
    AK = wrap_auto_tune(
      bl("surv.akritas", id = "akritas"),
      akritas.lambda = p_dbl(0, 1)
    ),

    CPH = bl("surv.coxph", id = "cph"),

    # Using the custom heuristic-based formula-generating function for splines on continuous features
    GAM = bl("surv.gam.cox", id = "gam", formula = "auto"),

    GLMN = wrap_auto_tune(
      bl("surv.cv_glmnet", id = "cv_glmnet", .encode = TRUE),
      cv_glmnet.alpha = p_dbl(0, 1)
    ),

    # MCP learner based on explicit reviewer request for MCP/SCAD, chose MCP based on maintainer input
    NCV = wrap_auto_tune(
      bl(
        "surv.cv_ncvsurv",
        id = "cv_ncv",
        .encode = TRUE,
        penalty = "MCP",
        # alpha = 1 -> MCP penalty
        alpha = 1
      ),
      # Tune on logscale in (1, 10]) https://github.com/pbreheny/ncvreg/discussions/57
      cv_ncv.gamma = p_dbl(1.0001, 10, logscale = TRUE)
    ),

    Pen = wrap_auto_tune(
      bl("surv.penalized", id = "penalized"),
      penalized.lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
      penalized.lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
    ),

    # Use grid search due to small + finite search space
    # AFT version needs to tune distributions within range of what's sensible/discussed with RS
    AFT = wrap_auto_tune(
      bl("surv.parametric", id = "parametric", discrete = TRUE),
      parametric.dist = p_fct(c("weibull", "exponential", "lognormal", "loglogistic")),
      use_grid_search = TRUE
    ),

    # Use grid search due to small + finite search space
    Flex = wrap_auto_tune(
      bl("surv.flexible", id = "flexible"),
      flexible.k = p_int(1, 10),
      use_grid_search = TRUE
    ),

    RFSRC = wrap_auto_tune(
      # Fixing ntime = 150 (current default) just to be explicit, as ranger's time.interest
      # is set to a non-default value and we ensure both use 150 time points for evaluation
      bl("surv.rfsrc", id = "rfsrc", ntree = 1000, ntime = 150),
      rfsrc.splitrule = p_fct(c("bs.gradient", "logrank")),
      rfsrc.mtry.ratio = p_dbl(0, 1),
      rfsrc.nodesize = p_int(1, 50),
      rfsrc.samptype = p_fct(c("swr", "swor")),
      rfsrc.sampsize.ratio = p_dbl(0, 1)
    ),

    RAN = wrap_auto_tune(
      # Adjusting time.interest (new as of 0.16.0) to 150, same as current RFSRC default
      bl("surv.ranger", id = "ranger", num.trees = 1000, time.interest = 150),
      ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
      ranger.mtry.ratio = p_dbl(0, 1),
      ranger.min.node.size = p_int(1, 50),
      ranger.replace = p_lgl(),
      ranger.sample.fraction = p_dbl(0, 1)
    ),

    CIF = wrap_auto_tune(
      bl("surv.cforest", id = "cforest", ntree = 1000),
      cforest.mtryratio = p_dbl(0, 1),
      cforest.minsplit = p_int(1, 50),
      cforest.mincriterion = p_dbl(0, 1),
      cforest.replace = p_lgl(),
      cforest.fraction = p_dbl(0, 1)
    ),

    # See https://github.com/mlr-org/mlr3extralearners/issues/383
    ORSF = wrap_auto_tune(
      bl(
        "surv.aorsf",
        id = "aorsf",
        n_tree = 1000,
        control_type = "fast",
        importance = "none" # Just for speed-up
      ),
      aorsf.mtry_ratio = p_dbl(0, 1),
      aorsf.leaf_min_events = p_int(5, 50),
      .extra_trafo = function(x, param_set) {
        x$aorsf.split_min_obs = x$aorsf.leaf_min_events + 5L
        x
      }
    ),

    RRT = wrap_auto_tune(
      bl("surv.rpart", id = "rpart"),
      rpart.minbucket = p_int(5, 50),
      use_grid_search = TRUE
    ),

    # Split based on family, omitting "cindex" which is different entirely
    MBSTCox = wrap_auto_tune(
      bl("surv.mboost", id = "mboost_cox", family = "coxph"),
      mboost_cox.mstop = p_int(10, 5000),
      mboost_cox.nu = p_dbl(0, 0.1),
      mboost_cox.baselearner = p_fct(c("bols", "btree"))
    ),

    MBSTAFT = wrap_auto_tune(
      bl("surv.mboost", id = "mboost_aft"),
      mboost_aft.family = p_fct(c("gehan", "weibull")),
      mboost_aft.mstop = p_int(10, 5000),
      mboost_aft.nu = p_dbl(0, 0.1),
      mboost_aft.baselearner = p_fct(c("bols", "btree"))
    ),

    # Does not use our inner resampling
    CoxB = bl(
      "surv.cv_coxboost",
      id = "coxboost",
      penalty = "optimCoxBoostPenalty",
      maxstepno = 5000,
      # Number of inner tuning folds: analogous to other AutoTuners
      K = conf$tuning$folds,
      .encode = TRUE
    ),

    # XGB/cox, uses breslow estimator internally via mlr3proba
    XGBCox = wrap_auto_tune(
      bl(
        "surv.xgboost.cox",
        id = "xgb_cox",
        tree_method = "hist",
        booster = "gbtree",
        early_stopping_rounds = 50,
        .encode = TRUE
      ),
      xgb_cox.nrounds = p_int(
        upper = 5000,
        tags = "internal_tuning",
        aggr = function(x) as.integer(mean(unlist(x)))
      ),
      xgb_cox.max_depth = p_int(1, 20),
      xgb_cox.subsample = p_dbl(0, 1),
      xgb_cox.colsample_bytree = p_dbl(0, 1),
      xgb_cox.eta = p_dbl(0, 1),
      xgb_cox.grow_policy = p_fct(c("depthwise", "lossguide"))
    ),

    # AFT version
    # - Tune distributions (as per JZ)
    XGBAFT = wrap_auto_tune(
      bl(
        "surv.xgboost.aft",
        id = "xgb_aft",
        tree_method = "hist",
        booster = "gbtree",
        early_stopping_rounds = 50,
        .encode = TRUE
      ),
      xgb_aft.nrounds = p_int(
        upper = 5000,
        tags = "internal_tuning",
        aggr = function(x) as.integer(mean(unlist(x)))
      ),
      xgb_aft.max_depth = p_int(1, 20),
      xgb_aft.subsample = p_dbl(0, 1),
      xgb_aft.colsample_bytree = p_dbl(0, 1),
      xgb_aft.eta = p_dbl(0, 1),
      xgb_aft.grow_policy = p_fct(c("depthwise", "lossguide")),
      xgb_aft.aft_loss_distribution = p_fct(c("normal", "logistic", "extreme")),
      xgb_aft.aft_loss_distribution_scale = p_dbl(0.5, 2.0)
    ),

    SSVM = wrap_auto_tune(
      bl(
        "surv.svm",
        id = "svm",
        type = "hybrid",
        diff.meth = "makediff3",
        # Set initial values but unused due to tuning
        gamma = 1,
        mu = 0,
        .encode = TRUE,
        .scale = TRUE
      ),
      svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
      svm.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
      svm.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
      svm.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x)
    )
  )

  imap(learners, function(l, id) l$id = id)

  cli::cli_h2("Cleaning up and adding to registry")

  if (measure$id == "isbs") {
    cli::cli_alert_warning("Skipping {.val MBSTAFT} for ISBS measure!")
    learners$MBSTAFT = NULL

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
    design = grid,
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

# Sanity checking at the end to ensure all learners are accounted for correctly
lrntab = load_lrntab()
experiments = summarizeExperiments(by = c("task_id", "learner_id"))

miss_ids = setdiff(lrntab$id, experiments$learner_id)
if (length(miss_ids) > 0) {
  cli::cli_warn("Mismatching learner IDs not in learners.csv: {.val {miss_ids}}")
}

cli::cli_alert_success("Added {.val {sum(experiments$.count)}} experiments to registry {.val {conf$reg_name}}")
cli::cli_li("{length(unique(experiments$learner_id))} learners")
cli::cli_li("{length(unique(experiments$task_id))} tasks")

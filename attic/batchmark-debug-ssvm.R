root = here::here()
source(file.path(root, "helpers.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
config_profile = Sys.getenv('R_CONFIG_ACTIVE', 'default')
cli::cli_alert_info("Loading config \"{config_profile}\"")
settings = config::get()

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
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")

# Create Registry ---------------------------------------------------------
reg_dir = file.path(root, settings$reg_name)

if (dir.exists(reg_dir)) {
  cli::cli_alert_danger("Deleting existing registry \"{settings$reg_name}\"!")
  unlink(reg_dir, recursive = TRUE)
}

cli::cli_alert_info("Creating new registry \"{settings$reg_name}\"!")
reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = settings$seed,
  packages = c("mlr3", "mlr3proba"), source = file.path(root, "helpers.R"))

# Create Tasks and corresponding instantiated Resamplings -----------------
set.seed(settings$seed)
files = dir(file.path(root, "code", "data"), pattern = "\\.rds$", full.names = TRUE)
names = stringi::stri_sub(basename(files), 1, -5)
tasks = resamplings = mlr3misc::named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  # Just for runtime estimation: Do simple holdout
  if (identical(c(settings$outer_folds, settings$inner_folds), c(1, 1))) {
    folds = 1
    resampling = rsmp("holdout", ratio = 4/5)$instantiate(task)
  } else {
    # normal CV
    folds = min(floor(task$nrow / settings$min_obs), settings$outer_folds)
    resampling = rsmp("cv", folds = folds)$instantiate(task)

    stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= settings$min_obs))

    save_resampling(resampling, names[i])
  }

  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, folds, resampling)
}

tasktab = save_tasktab(tasks)

# Base learner setup ------------------------------------------------------
#' Base learner with fallback + encapsulation, preprocessing pipeline and composition
#' @param key Learner key passed to `lrn()`.
#' @param ... Additional arguments passed to `lrn()`.
#' @param .encode Use `po("encode", method = "treatment")`? Set `TRUE` for e.g. XGBoost.
#' @param .scale Use `po("scale")`? Set `TRUE` for e.g. SSVM.
#' @param .form (`"ph"`) Passed to `distrcompositor` as `form = .form`.
#' @param .estimator (`"kaplan"`) Passed to `distrcompoistor` as `estimator = .estimator`.
bl = function(key, ..., .encode = FALSE, .scale = FALSE, .form = "ph", .estimator = "kaplan") {
  checkmate::assert_choice(.form, choices = c("ph", "aft"))
  checkmate::assert_choice(.estimator, choices = c("kaplan", "breslow"))
  cli::cli_h2("Constructing {key} (form = '{(.form)}')")
  if (.estimator == "breslow") cli::cli_alert_info("Using breslow estimator!")

  learner = lrn(key, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE,
                 method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # Needs to be consistent with each other but doesn't "do" anything, just formality in surv context
  fallback$predict_type = "crank"
  learner$predict_type = "crank"

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
    preproc = preproc %>>%
      po("scale")
  }

  # treatment encoding only for selected learners that don't support handling factors
  # Note: encoding is done for glmnet but not for coxph. Both are internally a
  # cox model, but glmnet does not do the treatment encoding automatically
  if (.encode) {
    preproc = preproc %>>%
      po("encode", method = "treatment")
  }

  # removeconstants: should constant features be introduced, they're dropped.
  #  - Done after treatment encoding
  # Stack preprocessing on top of learner + distr stuff.
  graph_learner = preproc %>>%
    po("removeconstants") %>>%
    # Compose $distr prediction, but only if underlying learner does not produce them itself
    ppl("distrcompositor", learner = learner, form = .form, estimator = .estimator, overwrite = FALSE) |>
    # Need to convert to GraphLearner
    as_learner()

  graph_learner$predict_type = "crank"
  if (TRUE) {
    graph_learner$fallback = fallback
    graph_learner$encapsulate = c(train = "callr", predict = "callr")
  }

  graph_learner
}

# AutoTuner -----------------------------------------------------------------------------------

#' Takes a base GraphLearner and search space and wraps it into AutoTuner
#' @param learner GRaphLearner returned by `bl()`.
#' @param ... Parameter search space, passed as `ps(...)`.
#' @param use_grid_search (`FALSE`) FOr learners with small categorical search spaces, it's more
#'   efficient to use grid search rather than to random search over e.g. 10 elements with 50 iters.
auto_tune = function(learner, ..., use_grid_search = FALSE) {
  learner = as_learner(learner)
  search_space = ps(...)
  if (is.null(search_space$trafo))
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))

  if (settings$inner_folds == 1) {
    # Runtime testing mode: holdout
    cli::cli_alert_warning("Using holdout inner resampling!")
    resampling = rsmp("holdout", ratio = 2/3)
  } else {
    # regular mode: (3-fold) CV
    resampling = rsmp("cv", folds = settings$inner_folds)
  }

  # Need to switch tuner/trm since some learners have very small search spaces
  # Here, we use grid search to efficiently search the limited (fewer than 50 unique HPCs)
  # search space and not waste compute by repeatedly evaluating the same HPCs

  # run_time: maximum time tuning is allowed to run, seconds (evaluated after all inner resampling iters)
  trm_runtime = trm("run_time", secs = settings$budget$runtime_hours * 60 * 60)
  # evals: budget set in settings.R: n_evals + k * dim_search_space
  trm_evals = trm("evals",
                  n_evals = settings$budget$evals_constant,
                  k = settings$budget$evals_multiplier)

  if (use_grid_search) {
    cli::cli_alert_info("Using grid search for tuning")

    # Use resolution that is normally greater than number of unique HPCs
    # For factors etc. this is fine, and for RRT (integer, 46 vals) this is also fine.
    # Also allows shorter runs during testing with small budget
    # Goal is to use grid_search with run_time terminator only

    # Account for pretest where we want 1 eval, so budget_constant may be 1
    # and multiplier 0
    if (settings$budget$evals_constant + settings$budget$evals_multiplier == 1) {
      grid_resolution = 1
    } else if (settings$budget$evals_multiplier == 0) {
      grid_resolution = settings$budget$evals_constant
    } else {
      grid_resolution = settings$budget$evals_multiplier
    }

    tuner = tnr("grid_search", resolution = grid_resolution)
    terminator = trm_runtime
  } else {
    # combo terminator https://bbotk.mlr-org.com/reference/mlr_terminators_combo.html
    terminator = trm("combo", list(trm_runtime, trm_evals), any = TRUE)
    tuner = tnr("random_search")
  }

  at = AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = resampling,
    # Measure will be set via global variable in loop
    measure = msr("surv.cindex", id = "harrell_c"),
    terminator = terminator,
    tuner = tuner,
    # Need tuning instance for archive, need archive to know if fallback was needed
    # Callback writes out archive, no need to store instance explicitly
    store_tuning_instance = TRUE,
    # Not needed: benchmark result of inner resamplings
    store_benchmark_result = TRUE,
    # Don't need models, only needed for variable imp etc. afaict
    store_models = TRUE
  )

  at$predict_type = "crank"


  at
}


# Assemble learners -------------------------------------------------------
learners = list(

  CPH = bl("surv.coxph")
  ,

  SSVM = auto_tune(
    bl("surv.svm", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3", .encode = TRUE, .scale = TRUE),
    surv.svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
    surv.svm.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
    surv.svm.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
    surv.svm.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x),
    .extra_trafo = function(x, param_set) {
      x$surv.svm.gamma.mu = c(x$surv.svm.gamma, x$surv.svm.mu)
      x$surv.svm.gamma = x$surv.svm.mu = NULL
      x
    }
  )

)

imap(learners, function(l, id) l$id = id)

# custom grid design (with instantiated resamplings)
grid = cross_join(list(task = tasks, learner = learners), sorted = FALSE)
grid$resampling = rep(resamplings, each = length(learners))
ids = batchmark(grid, store_models = settings$store$models)


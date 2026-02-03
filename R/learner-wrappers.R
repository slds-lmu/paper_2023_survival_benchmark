source(here::here("R/extralearners-ncv.R"))
source(here::here("R/extralearners-gam.R"))

# Base learner setup ------------------------------------------------------
#' Base learner with fallback + encapsulation, preprocessing pipeline and composition
#' @param key Learner key passed to `lrn()`.
#' @param id The (mandatory) base learner id needed for disambiguation in case a learner is used multiple times, such as `surv.mboost` for Cox and AFT variants.
#' @param ... Additional arguments passed to `lrn()`.
#' @param .encode Use `po("encode", method = "treatment")`? Set `TRUE` for e.g. XGBoost.
#' @param .scale Use `po("scale")`? Set `TRUE` for e.g. SSVM.
#' @param .threads `(integer(1))` Number of threads to use for parallel processing, set via `mlr3::set_threads`. If `NULL`, defaults to `max(1, conf$learners$threads)`
bl = function(key, id, ..., .encode = FALSE, .scale = FALSE, .threads = NULL) {
  cli::cli_h2("Constructing {.val {id}} from {.val {key}}")

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
    po("collapsefactors", no_collapse_above_prevalence = 0.05, target_level_count = 5)

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
    lrn(key, id = id, ...) |>
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
      train = conf$timeout$at_train * 3600,
      predict = conf$timeout$at_predict * 3600
    )
  } else {
    graph_learner$timeout = c(
      train = conf$timeout$bl_train * 3600,
      predict = conf$timeout$bl_predict * 3600
    )
  }

  # Used for XGBoost learners to enable internal tuning / early stopping using test set
  if ("validation" %in% graph_learner$properties) {
    cli::cli_alert_info("Setting validation field to {.val test}")
    set_validate(graph_learner, "test")
    checkmate::assert_true(graph_learner$validate == "test")
  }

  if (is.null(.threads)) {
    .threads = max(1, conf$learners$threads)
  }
  cli::cli_alert_info("Setting threads to {.val {(.threads)}}")
  set_threads(graph_learner, n = .threads)

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
  if (is.null(search_space$trafo)) {
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))
  }

  cli::cli_alert_warning("Using {.val {conf$tuning$resampling}} inner resampling!")

  resampling = switch(
    conf$tuning$resampling,
    "cv" = rsmp("cv", folds = conf$tuning$folds),
    "repeated_cv" = rsmp("repeated_cv", folds = conf$tuning$folds, repeats = conf$tuning$repeats),
    "holdout" = rsmp("holdout", ratio = conf$tuning$ratio)
  )

  # Need to switch tuner/trm since some learners have very small search spaces
  # Here, we use grid search to efficiently search the limited (fewer than 50 unique HPCs)
  # search space and not waste compute by repeatedly evaluating the same HPCs

  # run_time: maximum time tuning is allowed to run, seconds (evaluated after all inner resampling iters)
  trm_runtime = trm("run_time", secs = conf$budget$runtime_hours * 60 * 60)
  # evals: budget set in conf.R: n_evals + k * dim_search_space
  trm_evals = trm("evals", n_evals = conf$budget$evals_constant, k = conf$budget$evals_multiplier)

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

  # store context for callback to write archives with information needed for disambiguation
  # - path_dir: where to store the archives, context specific based on config
  # - learner_id: id of the **base** learner, needs to be set and unique!
  # - tuning_measure: id of the measure used for tuning
  callback_backup$state$path_dir = fs::path(conf$reg_dir, "tuning_archives")
  callback_backup$state$learner_id = learner$base_learner()$id
  callback_backup$state$tuning_measure = measure$id

  callback_archive_logs = callback_batch_tuning(
    id = "mlr3tuning.archive_logs",
    label = "Add tuning logs to archive for potential debugging",
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
    suppressWarnings(at$encapsulate("evaluate", lrn("surv.kaplan")))
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

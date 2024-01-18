library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
requireNamespace("mlr3extralearners")

# Base learner setup ------------------------------------------------------
#' Base learner with fallback + encapsulation, preprocessing pipeline and composition
#' @param key Learner key passed to `lrn()`.
#' @param ... Additional arguments passed to `lrn()`.
#' @param .form (`"ph"`) Passed to `distrcompositor` as `form = .form`.
#' @param .estimator (`"kaplan"`) Passed to `distrcompoistor` as `estimator = .estimator`.
bl = function(key, ..., .encode = FALSE, .scale = FALSE, .form = "ph", .estimator = "kaplan", .fallback = TRUE) {
  checkmate::assert_choice(.form, choices = c("ph", "aft"))
  checkmate::assert_choice(.estimator, choices = c("kaplan", "breslow"))
  cli::cli_h2("Constructing {key} (form = '{(.form)}')")
  if (.estimator == "breslow") cli::cli_alert_info("Using breslow estimator!")

  learner = lrn(key, ...)
  #fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE,
  #               method = "mean", overwrite = FALSE, graph_learner = TRUE)
  fallback = lrn("surv.kaplan")

  # Needs to be consistent with each other but doesn't "do" anything, just formality in surv context
  fallback$predict_type = "crank"
  learner$predict_type = "crank"

  # removeconstants: should constant features be introduced, they're dropped.
  #  - Done after treatment encoding
  # Stack preprocessing on top of learner + distr stuff.
  graph_learner = po("encode", method = "treatment") %>>%
    po("removeconstants") %>>%
    ppl("distrcompositor", learner = learner, form = .form, estimator = .estimator, overwrite = FALSE) |>
    as_learner()

  graph_learner$predict_type = "crank"

  if (.fallback) {
    graph_learner$fallback = fallback
    graph_learner$encapsulate = c(train = "callr", predict = "callr")
  }

  graph_learner
}

# Assemble learners -------------------------------------------------------

learners = list(
  XGBCox_fallback_breslow = bl(
    "surv.xgboost", tree_method = "hist", booster = "gbtree",
    objective = "survival:cox",
    max_depth = 2, eta = 0.3, nrounds = 20,
    .estimator = "breslow",
    .fallback = TRUE
  )

  ,

  XGBCox_fallback_kaplan =  bl(
    "surv.xgboost", tree_method = "hist", booster = "gbtree",
    objective = "survival:cox",
    max_depth = 2, eta = 0.3, nrounds = 20,
    .estimator = "kaplan",
    .fallback = TRUE
  )

  ,

  XGBCox_nofallback_breslow = bl(
    "surv.xgboost", tree_method = "hist", booster = "gbtree",
    objective = "survival:cox",
    max_depth = 2, eta = 0.3, nrounds = 20,
    .estimator = "breslow",
    .fallback = FALSE
  )
)

imap(learners, function(l, id) l$id = id)

grid = benchmark_grid(
  tasks = tsk("rats"),
  learners = learners,
  resamplings = rsmp("holdout", ratio = 4/5)
)

future::plan("multisession", workers = 3)
bmr = benchmark(grid, store_models = TRUE, store_backends = TRUE)

# errors column indicates there's something wrong
bmr

aggr = bmr$aggregate(measures = list(
    msr("surv.cindex", id = "harrell_c"),
    msr("surv.graf", id = "graf_proper_erv", proper = TRUE, ERV = TRUE)
  ), conditions = TRUE)

# KM = 0.5 and graf_proper with ERV = 0 imply that fallback was triggered
aggr


# Error in question I can't figure out -----------------------------------

learners$XGBCox_fallback_breslow$train(tasks$rats)
learners$XGBCox_fallback_breslow$predict(tasks$rats)
learners$XGBCox_fallback_breslow$state$log

# Doesn't appear without fallback
learners$XGBCox_nofallback_breslow$train(tasks$rats)
learners$XGBCox_nofallback_breslow$predict(tasks$rats)
learners$XGBCox_nofallback_breslow$state$log

# Nor with fallback but without breslow
learners$XGBCox_fallback_kaplan$train(tasks$rats)
learners$XGBCox_fallback_kaplan$predict(tasks$rats)
learners$XGBCox_fallback_kaplan$state$log



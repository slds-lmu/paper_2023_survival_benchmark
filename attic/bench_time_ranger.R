root = here::here()

# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# See also attic/_dependencies.R
# renv::restore(prompt = FALSE)

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")

# Create Tasks and corresponding instantiated Resamplings -----------------
set.seed(123)
files = dir(file.path(root, "code", "data"), pattern = "\\.rds$", full.names = TRUE)
names = stringi::stri_sub(basename(files), 1, -5)
tasks = mlr3misc::named_list(names)

idx = which(names %in% c("aids2", "grace", "child", "hdfail"))
files = files[idx]
names = names[idx]
tasks = resamplings = mlr3misc::named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  resamplings[[i]] = rsmp("holdout")$instantiate(task)
  tasks[[i]] = task
  rm(data, task)

}


# Base learner setup ------------------------------------------------------
bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE,
                 method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  fallback$predict_type = "crank"
  learner$predict_type = "crank"

  learner$fallback = fallback
  learner$encapsulate = c(train = "callr", predict = "callr")

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
  # Stack preprocessing on top of learner + distr stuff. 'form' as per RS
  preproc %>>%
    po("removeconstants") %>>%
    ppl("distrcompositor", learner = learner, form = "ph") |>
    # Need to convert to GraphLearner
    as_learner()
}


measures = list(
  harrell_c = msr("surv.cindex", id = "harrell_c"),
  dcalib = msr("surv.dcalib", id = "dcalib"),
  rcll = msr("surv.rcll", id = "rcll")
)

# Assemble learners -------------------------------------------------------

learners = list(
# survivalmodels::akritas
# https://raphaels1.github.io/survivalmodels/reference/akritas.html
AF = bl("surv.akritas", lambda = 0.5)
#surv.akritas.lambda = p_dbl(0, 1)

, CPH = bl("surv.coxph")

, RAN = bl("surv.ranger", num.trees = 500,
         splitrule = "logrank",
         min.node.size = 5,
         replace = TRUE)
# surv.ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
# surv.ranger.mtry.ratio = p_dbl(0, 1),
# surv.ranger.min.node.size = p_int(1, 50),
# surv.ranger.replace = p_lgl(),
# surv.ranger.sample.fraction = p_dbl(0, 1)
# CoxB = bl("surv.cv_coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 500, .encode = TRUE)
, XGB = bl("surv.xgboost", tree_method = "hist", booster = "gbtree", .encode = TRUE)
# surv.xgboost.max_depth = p_int(1, 20),
# surv.xgboost.subsample = p_dbl(0, 1),
# surv.xgboost.colsample_bytree = p_dbl(0, 1),
# surv.xgboost.nrounds = p_int(10, 5000),
# surv.xgboost.eta = p_dbl(0, 1),
# surv.xgboost.grow_policy = p_fct(c("depthwise", "lossguide"))

)

imap(learners, function(l, id) l$id = id)

grid = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = resamplings,
  paired = TRUE
)


# Benchmark? ----------------------------------------------------------------------------------

# future::plan("multisession", workers = 40)
# lgr::get_logger("mlr3")$set_threshold("debug")
#
# bmr = benchmark(
#   grid, store_models = TRUE, encapsulate = "callr"
# )
#
#
# scores = bmr$score(msrs(c("time_train", "time_predict")))
# aggr = bmr$aggregate(conditions = TRUE)
#
# if (!fs::dir_exists("tmp-ranger-timing")) fs::dir_create("tmp-ranger-timing")
# saveRDS(bmr, here::here("tmp-ranger-timing", "bmr.rds"))
#

# Different approach --------------------------------------------------------------------------

future::plan("multisession", workers = parallelly::availableCores())

# res = lapply(seq_len(nrow(grid)), \(i) {

ids = seq_len(nrow(grid))

grid = grid[2,]

# future.lapply didn't like the externalptr in grid or something
# res = future.apply::future_lapply(ids, \(i) {
res = furrr::future_pmap(grid, \(task, learner, resampling) {

  # learner = grid$learner[[i]]$clone()
  # task = grid$task[[i]]
  # resampling = grid$resampling[[i]]

  cli::cli_h2("Running {learner$id} on {task$id}")

  cli::cli_alert_info("Training...")
  tictoc::tic()
  learner$train(task, resampling$train_set(1))
  t_train = tictoc::toc()

  cli::cli_alert_info("Predicting...")
  tictoc::tic()
  pred = learner$predict(task, resampling$test_set(1))
  t_pred = tictoc::toc()

  cli::cli_alert_info("Scoring: Harrel's C")
  tictoc::tic()
  s_harrel = pred$score(measures$harrell_c)
  t_harrell = tictoc::toc()

  cli::cli_alert_info("Scoring: D-Calibration")
  tictoc::tic()
  s_dcalib = pred$score(measures$dcalib)
  t_dcalib = tictoc::toc()

  cli::cli_alert_info("Scoring: RCLL")
  tictoc::tic()
  s_rcll = pred$score(measures$rcll)
  t_rcll = tictoc::toc()


  data.table::data.table(
    learner = learner$id,
    task = task$id,
    time_train = t_train$toc,
    time_pred = t_pred$toc,
    time_harell_c = t_harrell$toc,
    time_dcalib = t_dcalib$toc,
    time_rcll = t_rcll$toc,
    harrell_c = s_harrel,
    dcalib = s_dcalib,
    rcll = s_rcll
  )
}, .options = furrr::furrr_options(seed = TRUE))

resdf = data.table::rbindlist(res)
if (!fs::dir_exists("tmp-ranger-timing")) fs::dir_create("tmp-ranger-timing")
saveRDS(resdf, here::here("tmp-ranger-timing", "resdf.rds"))

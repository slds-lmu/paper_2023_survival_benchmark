root = here::here()
reg_dir = file.path(root, "registry_dcalib_harrell_rcll")

###################################################################################################
### Packages
###################################################################################################
library(batchtools)
library(mlr3)
library(mlr3proba)
library(mlr3batchmark)
library(mlr3benchmark)
library(ggplot2)

reg = loadRegistry(reg_dir, writeable = TRUE, make.default = TRUE)

alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

results_dir <- here::here("tmp-harrell-rcll")
if (!file.exists(results_dir)) dir.create(results_dir)

###################################################################################################
### Reduce results
###################################################################################################
# Store eval measures for easier retrieval
measures_eval = get_measures_eval()

unique(alljobs$measure)

if (TRUE) {
  res = vector(mode = "list", length(unique(alljobs$measure)))
  names(res) = unique(alljobs$measure)

  for (meas in unique(alljobs$measure)) {
    ids = alljobs[measure == meas, ]

    tictoc::tic(msg = glue::glue("Reducing results: {meas}"))
    bmr = reduceResultsBatchmark(ids = findDone(ids, reg = reg), store_backends = TRUE, reg = reg)
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Aggregating results: {meas}"))
    aggr = bmr$aggregate(measures = measures_eval, conditions = TRUE)
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Saving results: {meas}"))
    saveRDS(bmr, file = glue::glue("{results_dir}/bmrs_runtime_est_{meas}.rds"))
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Saving aggregated results: {meas}"))
    saveRDS(aggr, glue::glue("{results_dir}/aggr_runtime_est_{meas}.rds"))
    tictoc::toc()

    res[[meas]] = list(bmr = bmr, aggr = aggr)
  }
}

if (!file.exists("tmp-harrell-rcll/aggr_combined.rds")) {
  aggr_harrell_c   = readRDS(here::here(results_dir, "aggr_runtime_est_harrell_c.rds"))
  aggr_graf_proper = readRDS(here::here(results_dir, "aggr_runtime_est_graf_proper.rds"))
  aggr_rcll        = readRDS(here::here(results_dir, "aggr_runtime_est_rcll.rds"))

  aggr_harrell_c[, tuning_measure := "harrell_c"]
  aggr_graf_proper[, tuning_measure := "graf_proper"]
  aggr_rcll[, tuning_measure := "rcll"]

  aggr = data.table::rbindlist(list(aggr_harrell_c, aggr_graf_proper, aggr_rcll))
  aggr[, resample_result := NULL]
  saveRDS(aggr, here::here(results_dir, "aggr_combined.rds"))
} else {
  aggr = readRDS(here::here(results_dir, "aggr_combined.rds"))
}

if (FALSE) {
  tictoc::tic("reading bmrs")
  bmr_harrell_c = readRDS(here::here(results_dir, "bmrs_runtime_est_harrell_c.rds"))
  bmr_graf_proper   = readRDS(here::here(results_dir, "bmrs_runtime_est_graf_proper.rds"))
  bmr_rcll         = readRDS(here::here(results_dir, "bmrs_runtime_est_rcll.rds"))
  tictoc::toc()
}


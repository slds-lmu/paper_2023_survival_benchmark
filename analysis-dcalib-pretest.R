root = here::here()
reg_dir = file.path(root, "registry_dcalib_test")

###################################################################################################
### Packages
###################################################################################################
library(batchtools)
library(mlr3batchmark)
library(ggplot2)

reg = loadRegistry(reg_dir, writeable = TRUE, make.default = TRUE)

alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

###################################################################################################
### Current State
###################################################################################################
# print(getStatus())
# print(unique(getErrorMessages()))

# ids = grepLogs(findErrors(), pattern = "incorrect number")
# summarizeExperiments(ids, by = c("learner_id"))
#
# ids = grepLogs(findErrors(), pattern = "Distribution")
# summarizeExperiments(ids, by = c("learner_id"))
#
# ids_expired = findExpired()
# if (nrow(ids_expired) > 0) {
#   summarizeExperiments(ids_expired, by = c("task_id"))
#   summarizeExperiments(ids_expired, by = c("learner_id"))
# }

#showLog(ids[1])

#ids = ijoin(findExpired(), findExperiments(prob.pars = task_id == "child"))
#showLog(ids[1])

###################################################################################################
### Reduce results
###################################################################################################
# Store eval measures for easier retrieval
measures_eval = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  msr("surv.rcll", id = "rcll"),

  msr("surv.graf", id = "graf_proper", proper = TRUE),
  msr("surv.graf", id = "graf_improper", proper = FALSE),

  msr("surv.dcalib", id = "dcalib"),

  msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  msr("surv.logloss", id = "logloss"),
  msr("surv.calib_alpha", id = "calib")
)
names(measures_eval) = mlr3misc::ids(measures_eval)

unique(alljobs$measure)

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
  saveRDS(bmr, file = glue::glue("tmp/bmrs_runtime_est_{meas}.rds"))
  tictoc::toc()

  tictoc::tic(msg = glue::glue("Saving aggregated results: {meas}"))
  saveRDS(aggr, glue::glue("tmp/aggr_runtime_est_{meas}.rds"))
  tictoc::toc()

  res[[meas]] = list(bmr = bmr, aggr = aggr)
}


root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################
library("batchtools")
library("mlr3batchmark")

reg = loadRegistry("registry", writeable = TRUE)

###################################################################################################
### Current State
###################################################################################################
print(getStatus())
print(getErrorMessages())

ids = grepLogs(findErrors(), pattern = "incorrect number")
summarizeExperiments(ids, by = c("learner_id"))

ids = grepLogs(findErrors(), pattern = "Distribution")
summarizeExperiments(ids, by = c("learner_id"))

ids = findExpired()
summarizeExperiments(ids, by = c("task_id"))

ids = ijoin(findExpired(), findExperiments(prob.pars = task_id == "child"))
showLog(ids[1])

###################################################################################################
### Reduce results
###################################################################################################
measures = msrs(c("surv.cindex", "surv.brier"))
bmr = reduceResultsBatchmark()
profvis::profvis({
aggr = bmr$aggregate(measures = measures[2], conditions = TRUE)
})
resamplings_with_error = aggr[errors > 0, nr]
mlr3viz::autoplot(bmr)
# bmr$resample_result(resamplings_with_error[1])$errors

root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################
library("batchtools")
library("mlr3batchmark")

reg = loadRegistry(reg_dir, writeable = TRUE)

###################################################################################################
### Current State
###################################################################################################
print(getStatus())
print(getErrorMessages())

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

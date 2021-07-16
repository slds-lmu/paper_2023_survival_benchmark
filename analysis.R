root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################
library(batchtools)
library(batchmark)

reg = loadRegistry(reg_dir)

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
aggr = bmr$aggregate(measures = measures, conditions = TRUE)
resamplings_with_error = aggr[errors > 0, nr]
mlr3viz::autoplot(bmr)
# bmr$resample_result(resamplings_with_error[1])$errors

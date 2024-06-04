# Circumvent srcref issue https://github.com/rstudio/renv/issues/1713
options("install.opts" = "--without-keep.source")

source("renv/activate.R")

# Trying to ensure learners don't use more resources than they should
# For e.g. XGBoost
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
# Unsure, MKL is an Intel-specific thing
Sys.setenv(MKL_NUM_THREADS = 1)

# Package-specific settings
try(data.table::setDTthreads(1))
#try(RhpcBLASctl::blas_set_num_threads(1))
#try(RhpcBLASctl::omp_set_num_threads(1))


options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE,
  batchtools.progress = FALSE
)

# Load script with helpers here to ensure its contents are available always
source("helpers.R")

# Make renv pick up learner dependencies
if (FALSE) {
  library("survival")
  library("pracma")
  library("glmnet")
  library("penalized")
  library("flexsurv")
  library("randomForestSRC")
  library("ranger")
  library("partykit")
  library("sandwich")
  library("coin")
  library("aorsf")
  library("rpart")
  library("mboost")
  library("xgboost")
  library("survivalsvm")
  library("pseudo")
  library("actuar") # For surv.parametric

  library("mlr3batchmark") # on CRAN now with v0.1.0
  library("distr6")
  library("mlr3proba")
  library("mlr3extralearners")
  library("CoxBoost")
  library("survivalmodels")

  library("callr")
  require("config")

  # For datasets
  library(frailtyHL)
  library(simPH)
  library(smcure)
  library(dynpred)

}

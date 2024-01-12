# Dependency list --------------------------------------------------------------------------------------------------
# Assembled semi-manually, with direct install commands for renv as the
# automatic pick up of dependencies was unreliable
if (FALSE) {
  renv::install("survival", prompt = FALSE)
  renv::install("pracma", prompt = FALSE)
  renv::install("glmnet", prompt = FALSE)
  renv::install("penalized", prompt = FALSE)
  renv::install("flexsurv", prompt = FALSE)
  renv::install("randomForestSRC", prompt = FALSE)
  renv::install("ranger", prompt = FALSE)
  renv::install("partykit", prompt = FALSE)
  renv::install("sandwich", prompt = FALSE)
  renv::install("coin", prompt = FALSE)

  # Require 0.1.2 due to split of internal functions and extralearners update
  # renv::install("aorsf", prompt = FALSE)
  renv::install("ropensci/aorsf", prompt = FALSE)
  
  renv::install("rpart", prompt = FALSE)
  renv::install("mboost", prompt = FALSE)
  renv::install("xgboost", prompt = FALSE)
  renv::install("survivalsvm", prompt = FALSE)
  renv::install("pseudo", prompt = FALSE)
  renv::install("actuar", prompt = FALSE) # For surv.parametric
  renv::install("binderh/CoxBoost", prompt = FALSE)

  renv::install("mlr3", prompt = FALSE)
  renv::install("mlr3learners", prompt = FALSE)
  renv::install("mlr3pipelines", prompt = FALSE)
  renv::install("mlr3tuning", prompt = FALSE)
  renv::install("batchtools", prompt = FALSE)

  # renv::install("mlr3batchmark", prompt = FALSE) # on CRAN now with v0.1.0
  renv::install("mlr-org/mlr3batchmark")

  # Non-CRAN pkgs for manual installation/updating
  renv::install("mlr-org/mlr3proba", prompt = FALSE)
  renv::install("mlr-org/mlr3extralearners", prompt = FALSE)
  renv::install("xoopR/distr6", prompt = FALSE)
  renv::install("RaphaelS1/survivalmodels", prompt = FALSE)

}


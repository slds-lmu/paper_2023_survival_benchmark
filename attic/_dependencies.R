# Dummy dep list --------------------------------------------------------------------------------------------------
# Only for renv to pick up on the implicit learner dependencies
# Assembled by running batchmark.R to create `learners` list, then
# lapply(mlr3::extract_pkgs(learners), function(x) cat("library(", x, ")\n", sep = ""))
if (FALSE) {
  library(survival)
  library(distr6)
  library(mlr3extralearners)
  library(pracma)
  library(survivalmodels)
  library(glmnet)
  library(penalized)
  library(flexsurv)
  library(randomForestSRC)
  library(ranger)
  library(partykit)
  library(sandwich)
  library(coin)
  library(aorsf)
  library(rpart)
  library(mboost)
  library(CoxBoost)
  library(xgboost)
  library(survivalsvm)
  library(reticulate)
  library(keras)
  library(pseudo)
  library(tensorflow)

  # Non-CRAN pkgs for manual installation/updating
  renv::install("mlr-org/mlr3batchmark")
  renv::install("mlr-org/mlr3proba")
  renv::install("mlr-org/mlr3extralearners")
  renv::install("binderh/CoxBoost")
  renv::install("RaphaelS1/survivalmodels")

  reticulate::conda_create(envname = "proba-bench", environment = "environment.yml")
  reticulate::use_condaenv("proba-bench", required = TRUE)
  survivalmodels::install_pycox()
  survivalmodels::install_keras(install_tensorflow = TRUE)
  survivalmodels::install_torch()
}

# Why is python.
reticulate::py_config()


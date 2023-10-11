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
  renv::install("aorsf", prompt = FALSE)
  renv::install("rpart", prompt = FALSE)
  renv::install("mboost", prompt = FALSE)
  renv::install("xgboost", prompt = FALSE)
  renv::install("survivalsvm", prompt = FALSE)
  renv::install("pseudo", prompt = FALSE)
  renv::install("actuar", prompt = FALSE) # For surv.parametric

  renv::install("mlr3learners", prompt = FALSE)
  renv::install("mlr3pipelines", prompt = FALSE)
  renv::install("mlr3tuning", prompt = FALSE)

  renv::install("mlr3batchmark", prompt = FALSE) # on CRAN now with v0.1.0
  # renv::install("mlr-org/mlr3batchmark")

  # Non-CRAN pkgs for manual installation/updating
  renv::install("mlr-org/mlr3proba", prompt = FALSE)
  renv::install("mlr-org/mlr3extralearners", prompt = FALSE)
  renv::install("binderh/CoxBoost", prompt = FALSE)
  renv::install("RaphaelS1/distr6", prompt = FALSE)
  renv::install("RaphaelS1/survivalmodels", prompt = FALSE)
}

# For quick local installation outside of renv -------------------------------
# using pak because its fast and does github
if (FALSE) {
  pak::pak("survival")
  pak::pak("pracma")
  pak::pak("glmnet")
  pak::pak("penalized")
  pak::pak("flexsurv")
  pak::pak("randomForestSRC")
  pak::pak("ranger")
  pak::pak("partykit")
  pak::pak("sandwich")
  pak::pak("coin")
  pak::pak("aorsf")
  pak::pak("rpart")
  pak::pak("mboost")
  pak::pak("xgboost")
  pak::pak("survivalsvm")
  pak::pak("pseudo")
  pak::pak("actuar") # For surv.parametric
  pak::pak("mlr3batchmark") # on CRAN now with v0.1.0

  pak::pak("RaphaelS1/distr6")
  pak::pak("mlr-org/mlr3proba")
  pak::pak("mlr-org/mlr3extralearners")
  pak::pak("binderh/CoxBoost")
  pak::pak("RaphaelS1/survivalmodels") # For akritas
}

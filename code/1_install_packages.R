# install if not already
sapply(c("survival", "checkmate", "data.table", "tidyverse",
         "GoFKernel", "extraDistr", "actuar", "pracma",
         "dplyr", "magrittr", "reticulate", "future", "future.apply"),
       function(x) {
         inst = requireNamespace(x, quietly = TRUE)
         if (!inst) {
           install.packages(x, repos = "http://cran.r-project.org")
         }
       })

# require latest stable versions
install.packages(c("mlr3misc", "mlr3pipelines", "mlr3tuning", "paradox"),
                 repos = "http://cran.r-project.org")

# require dev versions
remotes::install_github("alan-turing-institute/distr6", upgrade = "always")
remotes::install_github("mlr-org/mlr3proba", upgrade = "never")
remotes::install_github("mlr-org/mlr3", upgrade = "never")
remotes::install_github("mlr-org/mlr3learners", upgrade = "never")
remotes::install_github("mlr-org/mlr3extralearners", upgrade = "never")
remotes::install_github("mlr-org/mlr3benchmark", upgrade = "never")

# install learners
library(mlr3learners)
library(mlr3proba)
library(mlr3extralearners)
install_learners(c(
  "surv.kaplan",
  "surv.nelson",
  "surv.akritas",
  "surv.coxph",
  "surv.cv_glmnet",
  "surv.penalized",
  "surv.parametric",
  "surv.flexible",
  "surv.rfsrc",
  "surv.ranger",
  "surv.cforest",
  "surv.rpart",
  "surv.gbm",
  "surv.xgboost",
  "surv.blackboost",
  "surv.gamboost",
  "surv.glmboost",
  "surv.cv_coxboost",
  "surv.svm",
  "surv.coxtime",
  "surv.deephit",
  "surv.deepsurv",
  "surv.loghaz",
  "surv.pchazard",
  "surv.dnn"),
  repos = "http://cran.r-project.org"
)

survivalmodels::install_pycox()
survivalmodels::install_keras()

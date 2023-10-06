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
  renv::install("reticulate", prompt = FALSE)
  #renv::install("keras", prompt = FALSE)
  renv::install("pseudo", prompt = FALSE)
  #renv::install("tensorflow", prompt = FALSE)
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

  # Setting up python envs is hell ----
  # delete the old one if needed
  # reticulate::conda_remove("proba-bench")

  # Create from an existing environment.yaml
  # reticulate::conda_create(envname = "proba-bench", environment = "environment.yml")

  # Or a fresh one
  # reticulate::conda_create(envname = "proba-bench")

  # make sure to use it
  #reticulate::use_condaenv("proba-bench3.10", required = TRUE)

  # Trying to install learner deps
  #survivalmodels::install_pycox(method = "conda", install_torch = TRUE)

  # using keras install method because survivalmodels installed mismatching versions I think
  #survivalmodels::install_keras(install_tensorflow = TRUE)
  # installs correct version maybe? ^ installed 2.13, keras installs 2.11
  #keras::install_keras(method = "conda", version = "default", pip_ignore_installed = TRUE)

  # Installed manually in shell:
  # conda install mamba; mamba install pytorch pycox tensorflow keras

  # Save to environment.yml to restore on different machines
  # This failed in R for some reason, done in shell as well
  # conda env export > environment3.10.yml
  # reticulate::conda_export("proba-bench3.10", file = "environment3.10.yml")
}

# Had an issue where cuda complained about ptxas being too old, ($ which ptxas)
# suggests all my cuda's are > 11.3 or something
# but it said upgrading to 11.1 might help, which is concerning.
# conda install cuda -c nvidia
# according to
# https://github.com/google/jax/discussions/10327

# Why is python.
#reticulate::py_config()

#pypkgs <- reticulate::py_list_packages()
#pypkgs[grep("tensorfl|keras|pycox|pytorch|numpy", pypkgs$package), ]


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
  pak::pak("reticulate")
  #pak::pak("keras")
  #pak::pak("tensorflow")
  pak::pak("pseudo")
  pak::pak("actuar") # For surv.parametric
  pak::pak("mlr3batchmark") # on CRAN now with v0.1.0

  pak::pak("RaphaelS1/distr6")
  pak::pak("mlr-org/mlr3proba")
  pak::pak("mlr-org/mlr3extralearners")
  pak::pak("binderh/CoxBoost")
  pak::pak("RaphaelS1/survivalmodels")
}

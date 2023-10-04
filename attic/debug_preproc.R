root = here::here()
source(file.path(root, "settings.R"))

# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# See also attic/_dependencies.R
renv::restore(prompt = FALSE)

library("stringi")
library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")


# Tasks -------------------------------------------------------------------
set.seed(seed)
files = dir(file.path(root, "code", "data"), pattern = "\\.rds$", full.names = TRUE)
names = stri_sub(basename(files), 1, -5)
tasks = resamplings = named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  folds = min(floor(task$nrow / min_obs), outer_folds)
  resampling = rsmp("cv", folds = folds)$instantiate(task)
  stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= min_obs))

  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, folds, resampling)
}


# Create Learners -----------------------------------
bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  # DEBUG: Disable fallback to catch potentially fixable errors
  #fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  #fallback$predict_type = "crank"
  learner$predict_type = "crank"

  #learner$fallback = fallback
  #learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  # Added form as per RS
  g = ppl("distrcompositor", learner = learner, form = 'ph')

  if (.scale) {
    g = po("scale") %>>% g
  }

  if (.encode) {
    g = po("encode", method = "treatment") %>>% g
  }

  as_learner(po("fixfactors") %>>% po("collapsefactors", target_level_count = 5) %>>% g)
}


# Experimenting with different versions
bl_new = function(key, ..., .encode = FALSE, .encode_onehot = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  # DEBUG: Disable fallback to catch potentially fixable errors
  #fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  #fallback$predict_type = "crank"
  learner$predict_type = "crank"

  #learner$fallback = fallback
  #learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  preproc = po("fixfactors") %>>%
    po("collapsefactors", no_collapse_above_prevalence = 0.01)

  if (.scale) {
    preproc = preproc %>>% po("scale")
  }

  # encoding required for e.g. XGBoost and glmnet, but not applied to coxph(!)

  # one hot for some, with 2-level factors explicitly treatment encoded?
  if (.encode_onehot) {
    po("encode", method = "one-hot",
       affect_columns = selector_cardinality_greater_than(2),
       id = "onehot_") %>>%
    po("encode", method = "treatment",
       affect_columns = selector_type("factor"), id = "dummy_enc")
  }

  # For learners like glmnet where one-hot would be bad
  if (.encode) {
    preproc = preproc %>%
      po("encode", method = "treatment",
         affect_columns = selector_type("factor"))
  }

  # Removing constant vars as possibly introduced by previous steps
  preproc = preproc %>>%
    po("removeconstants")

  # Added form as per RS, should be last part of the pipeline
  preproc %>>%
    ppl("distrcompositor", learner = learner, form = "ph") |>
    as_learner()
}

# Example for learner that does its own treatment encoding
CPH = bl("surv.coxph")
CPH2 = bl_new("surv.coxph")

CPH$train(tasks$hdfail)
CPH2$train(tasks$hdfail)

tasks$hdfail

tasks$hdfail$data(cols = "model")[[1]] |>
  table() |>
  sort() |>
  rev() |>
  head(5) |>
  names()

CPH$model$surv.coxph$model
CPH$model$surv.coxph$train_task


CPH2$model$surv.coxph$model
CPH2$model$surv.coxph$train_task


# Example for learner that needs preprocessing (without tuning alpha here)
GLM = bl("surv.cv_glmnet", .encode = TRUE)



CPH$train(tasks$hdfail)
GLM$train(tasks$hdfail)


CPH$graph_model$output
GLM$model$encode$outtasklayout



# ppl from book -----------------------------------------------------------

factor_pipeline =
  po("removeconstants") %>>%
  po("collapsefactors", no_collapse_above_prevalence = 0.01) %>>%
  # po("encode", method = "one-hot",
  #    affect_columns = selector_cardinality_greater_than(2),
  #    id = "low_card_enc") %>>%
  po("encode", method = "treatment",
     affect_columns = selector_type("factor"), id = "dummy_enc")

# wrap learner with proba stuff for survival
learner = lrn("surv.coxph")
learner$predict_type = "crank"
glrn = factor_pipeline %>>% ppl("distrcompositor", learner = learner, form = "ph") |>
  as_learner()

glrn$graph_model

tasks$hdfail


glrn$train(tasks$hdfail)

glrn$graph$plot()
glrn$model$dummy_enc$outtasklayout

glrn$train(tasks$ALL)





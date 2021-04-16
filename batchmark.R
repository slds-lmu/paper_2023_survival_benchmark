library(mlr3)
library(mlr3misc)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3batchmark)

###################################################################################################
### Tasks
###################################################################################################
files = dir(file.path(here::here(), "code", "data"), pattern = "\\.rds$", full.names = TRUE)
tasks = lapply(files, function(f) {
  id = basename(f)
  id = substr(id, 1, nchar(id)-4)
  data = readRDS(f)
  as_task_surv(data, target = "time", event = "status", id = id)
})
names(tasks) = ids(tasks)

###################################################################################################
### Learners
###################################################################################################
learners = list(
  lrn("surv.kaplan", id = "class_nonpar_kaplan"),
  lrn("surv.akritas", id = "class_nonpar_akritas"),
  lrn("surv.coxph", id = "class_semipar_coxph"),
  lrn("surv.cv_glmnet", id = "class_semipar_cvglmnet"),
  lrn("surv.penalized", id = "class_semipar_penalized"),
  lrn("surv.parametric", id = "class_par_param", type = "aft"),
  lrn("surv.flexible", id = "class_par_flex"),
  lrn("surv.rfsrc", id = "ml_ranfor_rfsrc_brier", splitrule = "bs.gradient")
)

# set list names to ids
names(learners) = ids(learners)

# enable encapsulation and fallback for all learners
fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean",
  overwrite = FALSE, graph_learner = TRUE)

learners = lapply(learners, function(l) {
  l$encapsulate = c(train = "evaluate", predict = "evaluate")
  l$fallback = fallback
  l
})


###################################################################################################
### Resampling
###################################################################################################
outer = rsmp("cv", folds = 3)

###################################################################################################
### Registry
###################################################################################################
grid = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = list(outer)
)

reg = batchtools::makeExperimentRegistry(tempfile(), work.dir = here::here(), seed = 123)
batchmark(grid, store_models = FALSE)

unnest(batchtools::getJobTable(), c("prob.pars", "algo.pars"))

if (FALSE) {
  ids = batchtools::findExperiments(prob.pars = task_id == "lung", algo.pars = learner_id == "class_par_flex")
  ids = batchtools::ajoin(ids, batchtools::findDone())

  ids = batchtools::findNotDone()[sample(.N, 5)]
  batchtools::submitJobs(ids)

}

bmr = reduceResultsBatchmark()
aggr = bmr$aggregate(conditions = TRUE)
resamplings_with_error = aggr[errors > 0, nr]
bmr$resample_result(resamplings_with_error[1])$errors

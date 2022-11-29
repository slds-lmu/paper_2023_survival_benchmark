root = here::here()
source(file.path(root, "settings.R"))

library("stringi")
library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")

reg = loadRegistry(reg_dir, writeable = TRUE)
res = list(walltime = 4 * 3600, memory = 4096)

# All jobs ----------------------------------------------------------------

alljobs = unnest(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

# alljobs[, .(count = .N), by = learner_id]
alljobs[, .(count = .N), by = task_id]

alljobs[repl == 1 & task_id == "wtd"] |>
  submitJobs(resources = res)


# ezpz jobs probably? -----------------------------------------------------

alljobs[task_id %in% c("veteran", "lung", "mgus", "wbc1", "e1684") & learner_id %in% c("KM", "NL", "CPH", "GLM", "RFSRC", "RAN", "ORSF", "RRT")] |>
  submitJobs(res = res)

alljobs[task_id %in% c("veteran", "lung", "mgus", "wbc1") & learner_id %in% c("KM", "NL", "CPH", "GLM", "RFSRC", "RAN", "ORSF")] |>
  submitJobs(res = res)

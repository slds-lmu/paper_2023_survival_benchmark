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

# alljobs[, .(count = .N), by = task_id]
# alljobs[, .(count = .N), by = .(task_id, learner_id, measure)]

small_tasks <- c("veteran", "lung", "mgus", "wbc1", "e1684")
stable_learners <- c("KM", "NL", "CPH", "GLM", "RFSRC", "RAN", "ORSF")
dl_learners <-  c("CoxT", "DH", "DS", "LH", "PCH", "DNN")

# ezpz jobs probably? -----------------------------------------------------

# alljobs[task_id %in% small_tasks & learner_id %in% stable_learners & grepl("rcll", measure)] |>
#   submitJobs(res = res)
#
# # DL learners -----------------------------------------------------------------------------------------------------
#
# alljobs[task_id %in% small_tasks & learner_id %in% dl_learners & grepl("rcll", measure)] |>
#   submitJobs(resources = res)

alljobs[grepl("rcll", measure)] |>
  findNotSubmitted() |>
  submitJobs(resources = res)

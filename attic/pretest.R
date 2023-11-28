source(here::here("settings.R"))

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(reg_dir, writeable = TRUE)

# All jobs ----------------------------------------------------------------

alljobs = unnest(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

alljobs[, .(count = .N), by = task_id]
alljobs[, .(count = .N), by = .(task_id, learner_id, measure)]

if (FALSE) {
  problem_tasks <- c("bladder0", "hdfail", "whas", "aids2")
  problem_learners <- c("AF","CPH", "GLM", "Par", "Flex", "CoxB", "RAN")

  problem_jobs <- alljobs[task_id %in% problem_tasks & learner_id %in% problem_learners & grepl("rcll", measure)]
  nrow(problem_jobs)

  problem_jobs <- ijoin(problem_jobs, findExperiments(repls = 1))
  nrow(problem_jobs)

  problem_jobs |>
    findNotSubmitted() |>
    submitJobs(resources = resources)
}

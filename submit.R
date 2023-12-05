root = here::here()
source(file.path(root, "settings_trial_mode.R"))
# reg_dir = file.path(root, "registry")

library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(reg_dir, writeable = TRUE)

# Expecting 561 task x learner combinations, 5 outer folds, 3 inner folds
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

tasktab = read.csv(here::here("attic/tasktab.csv"))
resource_tab = read.csv(here::here("attic/resource_est.csv"))
resource_tab = resource_tab[, c("learner_id", "task_id", "measure", "hours", "total_h", "mem_gb")]

alljobs = ljoin(alljobs, tasktab, by = "task_id")
alljobs = ljoin(alljobs, resource_tab, by = c("task_id", "learner_id", "measure"))
data.table::setkey(alljobs, job.id)

# Estimated runtimes might be missing, so "impute" as 75% quantile for not entirely implausible values
alljobs[, hours   := ifelse(is.na(hours),   quantile(hours, na.rm = TRUE,   probs = 0.75), hours),   by = .(task_id)]
alljobs[, total_h := ifelse(is.na(total_h), quantile(total_h, na.rm = TRUE, probs = 0.75), total_h), by = .(task_id)]
alljobs[, mem_gb  := ifelse(is.na(mem_gb),  quantile(mem_gb, na.rm = TRUE,  probs = 0.75), mem_gb),  by = .(task_id)]

# Non-tuned learners --------------------------------------------------------------------------
# learners without inner resampling (KM, CoxBoost, ..) so not technically untuned but no inner resampling
# where measure == "dcalib,harrell_c,rcll"

jobs_untuned = alljobs[grepl(",", measure), ]
jobs_untuned[, chunk := lpt(total_h, 10)]
print(jobs_untuned[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])

jobs_untuned[learner_id == "CoxB", ]

# Tuning on Harrell's C -----------------------------------------------------------------------
jobs_harrell = alljobs[measure == "harrell_c", ]
jobs_harrell[, chunk := lpt(total_h, 50)]
print(jobs_harrell[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])


jobs_harrell[learner_id %in% c("Par", "RRT", "Flex", "XGB") & uniq_t_rank < 3, ] |>
  submitJobs()

# Tuning on RCLL -------------------------------------------------------------------
jobs_rcll = alljobs[measure == "rcll", ]
jobs_rcll[, chunk := lpt(total_h, 10)]
print(jobs_rcll[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])


# # Tuning on D-Calibration ---------------------------------------------------------------------
# jobs_dcalib = alljobs[measure == "dcalib", ]
# jobs_dcalib[, chunk := lpt(total_h, 10)]
# print(jobs_dcalib[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])

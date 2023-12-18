root = here::here()
# source(file.path(root, "settings_trial_mode.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()

library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg_dir = file.path(root, settings$reg_name)
reg = loadRegistry(settings$reg, writeable = TRUE)

# Expecting 544 task x learner combinations, 5 outer folds, 2 tuning measures
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = collect_job_table(reg = reg)

# Estimated runtimes might be missing, so "impute" as 75% quantile for not entirely implausible values
alljobs[, hours   := ifelse(is.na(hours),   quantile(hours, na.rm = TRUE,   probs = 0.75), hours),   by = .(task_id)]
alljobs[, total_h := ifelse(is.na(total_h), quantile(total_h, na.rm = TRUE, probs = 0.75), total_h), by = .(task_id)]
alljobs[, mem_gb  := ifelse(is.na(mem_gb),  quantile(mem_gb, na.rm = TRUE,  probs = 0.75), mem_gb),  by = .(task_id)]

# Set default resources?
alljobs[, walltime := settings$resources$walltime]
alljobs[, memory := settings$resources$memory]

# Non-tuned learners --------------------------------------------------------------------------
# learners without inner resampling (KM, CoxBoost, ..) so not technically untuned but no inner resampling
# where measure == "dcalib,harrell_c,rcll"

jobs_untuned = alljobs[grepl(",", measure), ]
jobs_untuned[, chunk := lpt(total_h, 50)]
print(jobs_untuned[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])

# jobs_untuned[learner_id == "CoxB", ]

# Tuning on Harrell's C -----------------------------------------------------------------------
jobs_harrell = alljobs[measure == "harrell_c", ]
jobs_harrell[, chunk := lpt(total_h, 100)]
print(jobs_harrell[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])


# Tuning on RCLL -------------------------------------------------------------------
jobs_rcll = alljobs[measure == "rcll", ]
jobs_rcll[, chunk := lpt(total_h, 10)]
print(jobs_rcll[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk])

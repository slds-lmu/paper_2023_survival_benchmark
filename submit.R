root = here::here()

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()

mem_beartooth = 4681
mem_teton = 4096
mem_knl = 5457

library("batchtools")
library("mlr3batchmark")
source(here::here("helpers.R"))

# Assumes batchmark.R is run beforehand
reg = loadRegistry(settings$reg_dir, writeable = TRUE)

# Expecting 576 task (32) x learner (19) combinations, 5 outer folds (except "veteran"), 2 tuning measures
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = collect_job_table(reg = reg)

# Estimated runtimes might be missing, so "impute" as 75% quantile for not entirely implausible values
alljobs[, hours   := ifelse(is.na(hours),   quantile(hours, na.rm = TRUE,   probs = 0.75), hours),   by = .(task_id)]
alljobs[, total_h := ifelse(is.na(total_h), quantile(total_h, na.rm = TRUE, probs = 0.75), total_h), by = .(task_id)]
alljobs[, mem_gb  := ifelse(is.na(mem_gb),  quantile(mem_gb, na.rm = TRUE,  probs = 0.75), mem_gb),  by = .(task_id)]

# Non-tuned learners --------------------------------------------------------------------------
# learners without inner resampling (KM, CoxBoost, ..) so not technically untuned but no inner resampling
# where measure == "harrell_c,rcll"

jobs_untuned = alljobs[grepl(",", measure), ]
jobs_untuned[, chunk := lpt(total_h, 50)]
chunks_untuned = jobs_untuned[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk]

# Tuning on Harrell's C -----------------------------------------------------------------------
jobs_harrell = alljobs[measure == "harrell_c", ]
jobs_harrell[, chunk := lpt(total_h, 100)]
chunks_harrell = jobs_harrell[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk]

# Tuning on RCLL -------------------------------------------------------------------
jobs_rcll = alljobs[measure == "rcll", ]
jobs_rcll[, chunk := lpt(total_h, 100)]
chunks_rcll = jobs_rcll[, list(total_h = sum(total_h), mem = sum(mem_gb), count = .N), by = chunk]

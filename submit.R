root = here::here()

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()

library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg_dir = file.path(root, settings$reg_name)
reg = loadRegistry(settings$reg, writeable = TRUE)

# Expecting 576 task (32) x learner (19) combinations, 5 outer folds (except "veteran"), 2 tuning measures
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = collect_job_table(reg = reg)

# Preprocess results --------------------------------------------------------------------------
source(here::here("helpers.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()

library(batchtools)
library(mlr3batchmark)
library(mlr3benchmark)
library(mlr3proba)
requireNamespace("mlr3extralearners")
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(ggplot2)

reg_dir = here::here(settings$reg_name)
reg = suppressWarnings(loadRegistry(reg_dir, writeable = TRUE))

alljobs = collect_job_table(reg)
result_path = here::here("results")

# Reassembling tuning archives ----------------------------------------------------------------
if (!fs::file_exists(fs::path(result_path, settings$reg_name, "archives-with-logs.rds"))) {
  cli::cli_alert_info("Reassembling tuning archives including logs")
  tictoc::tic()
  archives = reassemble_archives(reg_name = settings$reg_name, result_path = result_path, keep_logs = TRUE)
  tictoc::toc()
}

if (!fs::file_exists(fs::path(result_path, settings$reg_name, "archives-no-logs.rds"))) {
  cli::cli_alert_info("Reassembling tuning archives without logs")
  tictoc::tic()
  archives = reassemble_archives(reg_name = settings$reg_name, result_path = result_path, keep_logs = FALSE)
  tictoc::toc()
}

# Reducing results ----------------------------------------------------------------------------

# Store eval measures for easier retrieval
msr_tbl = measures_tbl()
measures_eval = msr_tbl$measure
measures_eval_ids = msr_tbl$id

# Task ids where all jobs are done
# done_tasks = check_job_state(byvars = c("task_id"))[expired == "—"]$task_id
# done_task_ids = alljobs[task_id %in% done_tasks]

# Creating bmr and bma for jobs tuned with harrell's c and untuned/coxboost, saving to result_path
collect_results(
  reg_name = settings$reg_name,
  # id_filter = done_task_ids,
  tuning_measure = "harrell_c",
  measures_eval = measures_eval
)

# Same for RCLL
collect_results(
  reg_name = settings$reg_name,
  # id_filter = done_task_ids,
  tuning_measure = "rcll",
  measures_eval = measures_eval
)


# Post-processing -----------------------------------------------------------------------------
bma_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bma_harrell_c.rds"))
bma_rcll       = readRDS(fs::path(result_path, settings$reg_name, "bma_rcll.rds"))

# Excluding SSVM results as there are no usable ones
bma_harrell_c = remove_results(bma_harrell_c, learner_id_exclude = "SSVM")
bma_rcll      = remove_results(bma_rcll,      learner_id_exclude = "SSVM")

# For consistency and disambiguation of some abbreviations
bma_harrell_c = rename_learners(bma_harrell_c)
bma_rcll      = rename_learners(bma_rcll)


saveRDS(bma_harrell_c, file = fs::path(result_path, settings$reg_name, "bma_clean_harrell_c.rds"))
saveRDS(bma_rcll,      file = fs::path(result_path, settings$reg_name, "bma_clean_rcll.rds"))

# Adding all aggregated scores for both tuning measures to a simple DT
# and add additional metadata for grouping
bma = combine_bma(bma_harrell_c, bma_rcll) |>
  add_learner_groups()

# Coarse manual checks to ensure roughly the correct shape
checkmate::assert_true(length(unique(bma$task_id)) == 32)
checkmate::assert_true(length(unique(bma$learner_id)) == 17)
checkmate::assert_set_equal(unique(bma$tuned), c("harrell_c", "rcll"))

# Technically not a bma anymore but useful nonetheless
saveRDS(bma, file = fs::path(result_path, settings$reg_name,  "bma_full.rds"))

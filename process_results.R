# Preprocess results --------------------------------------------------------------------------
# This script processes the batchtools registry and creates the BenchmarkResult (bmr) objects
# for both tuning measures the benchmark was run on and creates the derivative result files which
# are much easier to handle as they have much smaller file sizes.
# The bmr files are upwards of 5GB in size, per file.
source(here::here("helpers.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
# "BEartooth" denotes the config for results retrieved from cluster (other than e.g. local trial runs)
settings = config::get(value = "beartooth")

library(batchtools)
library(mlr3proba)
requireNamespace("mlr3extralearners")
library(mlr3batchmark)
library(mlr3benchmark)
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)

# Store eval measures for easier retrieval
msr_tbl = measures_tbl()
measures_eval = get_measures_eval()

# Reassembling tuning archives ----------------------------------------------------------------
# Archives with logs are very large objects of questionable utility, version without logs should suffice
if (!fs::file_exists(fs::path(settings$result_path, "archives-with-logs.rds"))) {
  cli::cli_alert_info("Reassembling tuning archives including logs")
  tictoc::tic()
  archives = reassemble_archives(settings, keep_logs = TRUE)
  tictoc::toc()
}

if (!fs::file_exists(fs::path(settings$result_path, "archives-no-logs.rds"))) {
  cli::cli_alert_info("Reassembling tuning archives without logs")
  tictoc::tic()
  archives = reassemble_archives(settings, keep_logs = FALSE)
  tictoc::toc()
}

# Create zipped collection of tuning archive CSVs
if (!fs::file_exists(fs::path(settings$result_path, "archives.zip"))) {

  tictoc::tic()
  convert_archives_csv(settings)

  zip_out = fs::path(settings$result_path, "archives.zip")

  utils::zip(
    zipfile = zip_out,
    files = fs::dir_ls(fs::path(settings$result_path, "tuning_archives")),
    flags = "-rD9j"
  )

  tictoc::toc()
}

# Reducing results ----------------------------------------------------------------------------
# Also includes creation of bma

# Task ids where all jobs are done
# done_tasks = check_job_state(byvars = c("task_id"))[expired == "—"]$task_id
# done_task_ids = alljobs[task_id %in% done_tasks]

# Creating bmr and bma for jobs tuned with harrell's c and untuned/coxboost,
# saving to result_path
collect_results(
  settings,
  tuning_measure = "harrell_c",
  measures_eval = measures_eval
)

# Same for RCLL
collect_results(
  settings,
  tuning_measure = "rcll",
  measures_eval = measures_eval
)

# Scoring is even more RAM intensive and should be done deliberately
# Only for some measures.
if (FALSE) {
  score_bmr(
    settings,
    measure = msr_tbl[!(erv), measure],
    tuning_measure = "harrell_c",
    nthreads = 2
  )

  score_bmr(
    settings,
    measure = msr_tbl[!(erv), measure],
    tuning_measure = "rcll",
    nthreads = 2
  )
}

# Similarly, manually aggregating and saving results separately takes a while
# and is not strictly necessary if collect_results() created a bma already.
if (FALSE) {
  aggr_bmr(
    settings,
    measure = measures_eval,
    tuning_measure = "harrell_c",
    nthreads = 2
  )

  aggr_bmr(
    settings,
    measure = measures_eval,
    tuning_measure = "rcll",
    nthreads = 2
  )
}

# Post-processing -----------------------------------------------------------------------------
bma_harrell_c  = readRDS(fs::path(settings$result_path, "bma_harrell_c.rds"))
bma_rcll       = readRDS(fs::path(settings$result_path, "bma_rcll.rds"))

# Excluding SSVM results as there are no usable ones
bma_harrell_c = remove_results(bma_harrell_c, learner_id_exclude = "SSVM")
bma_rcll      = remove_results(bma_rcll,      learner_id_exclude = "SSVM")

# For consistency and disambiguation of some abbreviations
bma_harrell_c = rename_learners(bma_harrell_c)
bma_rcll      = rename_learners(bma_rcll)

# Saving cleaned results
saveRDS(bma_harrell_c, file = fs::path(settings$result_path, "bma_clean_harrell_c.rds"))
saveRDS(bma_rcll,      file = fs::path(settings$result_path, "bma_clean_rcll.rds"))

# Adding all aggregated scores for both tuning measures to a simple DT
# and add additional metadata for grouping
bma = combine_bma(bma_harrell_c, bma_rcll) |>
  add_learner_groups()

# Coarse manual checks to ensure roughly the correct shape
checkmate::assert_true(length(unique(bma$task_id)) == 32)
checkmate::assert_true(length(unique(bma$learner_id)) == 17)
checkmate::assert_set_equal(unique(bma$tuned), c("harrell_c", "rcll"))

# Technically not a bma anymore but useful nonetheless
saveRDS(bma, file = fs::path(settings$result_path,  "aggr_scores.rds"))

# Write CSV with aggregated scores
write.csv(bma, file = fs::path(settings$result_path, "aggr_scores.csv"))

# Aggregating scores --------------------------------------------------------------------------
# This requires score_bmr() above!

scores_harrell_c = combine_scores_aggrs(settings, tuning_measure = "harrell_c", type = "scores")
scores_rcll      = combine_scores_aggrs(settings, tuning_measure = "rcll",      type = "scores")

scores = rbind(scores_rcll, scores_harrell_c)
# Exclude broken SSVM results, rename learners for consistency
scores = scores |>
  remove_results(learner_id_exclude = "SSVM") |>
  rename_learners() |>
  add_learner_groups()


# Handle warnings and errors list columns
warning_lengths = vapply(scores$warnings, length, integer(1))
sum(warning_lengths > 0)
# No warnings, dropping column
scores[, warnings := NULL]

error_lengths = vapply(scores$errors, length, integer(1))
sum(error_lengths > 0)

# Converting errors to character to make it more handleable
scores[, errors := sapply(errors, function(x) paste(x, collapse = "\n"))]

# Very strict assertion just to make sure, will need adjustment if minor things change
checkmate::assert_data_table(scores, any.missing = FALSE, nrows = 5406, ncols = 22)


saveRDS(scores, file = fs::path(settings$result_path,  "scores.rds"))

# Write CSV
write.csv(scores, file = fs::path(settings$result_path, "scores.csv"))


library(batchtools)
library(mlr3)
library(mlr3proba)
library(data.table)

if (!fs::dir_exists(conf$result_path)) {
  fs::dir_create(conf$result_path, recurse = TRUE)
}

save_obj <- function(obj, name, suffix = NULL, sep = "_") {
  if (!is.null(suffix)) {
    suffix <- paste(suffix, collapse = sep)
    name <- paste0(name, sep, suffix)
  }
  path <- fs::path(conf$result_path, name, ext = "rds")
  cli::cli_alert_info("Saving {.val {deparse(substitute(obj))}} to {.file {fs::path_rel(path)}}")
  saveRDS(obj, path)
}

# Store eval measures for easier retrieval
msr_tbl = measures_tbl()

tictoc::tic("Full result processing")
reg <- loadRegistry(conf$reg_dir, writeable = FALSE, work.dir = here::here())
tab <- collect_job_table(
  keep_columns = c("job.id", "repl", "tags", "task_id", "learner_id", "time.running", "mem.used"),
  resource_est_file = ""
)
tab[, time.hours := as.numeric(time.running, unit = "hours")]
data.table::setkey(tab, job.id)

runtimes <- tab[
  !is.na(time.running),
  .(
    mean_time_hours = mean(time.hours),
    min_time_hours = min(time.hours),
    max_time_hours = max(time.hours),
    mean_mem_used = mean(mem.used, na.rm = TRUE)
  ),
  by = .(learner_id, task_id)
]

save_obj(runtimes, name = "runtimes")
save_obj(tab, name = "jobs")

cli::cli_h1("Processing registry")
print(getStatus())

tune_measures = unique(tab$measure)
learners = unique(tab$learner_id)
learner_measure_tab = unique(tab[, .(measure, learner_id)])

# For runtime estimate runs we only score using the tuning measures
if (config::is_active("runtime")) {
  msr_tbl = msr_tbl[id %in% tune_measures, ]
}


# Result processing -----------------------------------------------------------------
for (tune_measure in tune_measures) {
  cli::cli_h2("Processing results for {.val {tune_measure}}")

  # subset eval measures to those applicable for tuning measure
  # Using Discrimination measures only when tuned on Harrel's C
  # and scoring rules plus calibration measures otherwise
  measures = switch(
    tune_measure,
    "harrell_c,isbs" = msr_tbl[, measure],
    "harrell_c" = msr_tbl[type == "Discrimination", measure],
    "isbs" = msr_tbl[type != "Discrimination", measure]
  )

  # For runtime estimates we also score with timing measures
  if (config::is_active("runtime")) {
    measures = c(measures, msrs(c("time_train", "time_predict")))
  }

  cli::cli_alert_info("Using eval measures {.val {mlr3misc::ids(measures)}}")

  for (learner in learners) {
    cli::cli_h3("Processing results for {.val {learner}}")

    # Assemble relevant job.ids
    ids_all = tab[learner_id == learner & measure == tune_measure, ]
    ids = ijoin(findDone(), ids_all)

    cli::cli_inform("Found {.val {nrow(ids)}} / {.val {nrow(ids_all)}} completed jobs")
    # skip if there's no completed jobs
    if (nrow(ids) == 0) next

    tictoc::tic(msg = "Reducing results")
    # Disabling the progress bar for speedup with many jobs
    options(batchtools.progress = FALSE)
    bmr <- mlr3batchmark::reduceResultsBatchmark(ids, store_backends = TRUE)
    options(batchtools.progress = TRUE)
    tictoc::toc()

    cli::cli_h3("Scoring and aggregating")

    tictoc::tic(msg = "Scoring results")
    scores <- bmr$score(measures, conditions = TRUE)
    # scores <- as.data.table(scores)
    scores[, task := NULL]
    scores[, learner := NULL]
    # scores[, resampling := NULL]
    # scores[, resampling_id := NULL]
    # scores[, uhash := NULL]

    scores[, tune_measure := ..tune_measure]
    # Count errors and warnings (either empty list = 0 or list of error messages), but preserve originals just in case
    scores[, errors_cnt := vapply(errors, length, FUN.VALUE = integer(1))]
    scores[, warnings_cnt := vapply(warnings, length, FUN.VALUE = integer(1))]

    tictoc::toc()

    tictoc::tic(msg = "Aggregating results")
    aggr <- bmr$aggregate(measures, conditions = TRUE)
    aggr[, tune_measure := ..tune_measure]
    aggr[, resampling_id := NULL]
    tictoc::toc()

    tictoc::tic(msg = "Saving to disk: scores, aggr")
    save_obj(scores, name = "scores", suffix = c(tune_measure, learner))
    save_obj(aggr, name = "aggr", suffix = c(tune_measure, learner))
    tictoc::toc()

    tictoc::tic("Collecting garbage just in case")
    gc(reset = TRUE)
    rm(bmr, scores, aggr)
    memory.mult = c(if (.Machine$sizeof.pointer == 4L) 28L else 56L, 8L)
    mem.used = sum(gc()[, 1L] * memory.mult)
    cli::cli_alert_info("Used {.val {prettyunits::pretty_bytes(mem.used)}}")
    tictoc::toc()
  }
}


cli::cli_h2("Combining results")

# Combine by tuning measure first
for (tune_measure in tune_measures) {
  cli::cli_alert_info("Combining results for {.val {tune_measure}}")

  tictoc::tic("scores")
  # Try to find score files expected given the current learner/measure combination
  current_learners = learner_measure_tab[measure == tune_measure, learner_id]
  score_files = fs::path(conf$result_path, glue::glue("scores_{tune_measure}_{current_learners}.rds"))
  score_files = mlr3misc::keep(score_files, fs::file_exists)

  if (length(score_files) == 0) {
    cli::cli_alert_danger("No score files for {.val {tune_measure}}, skipping")
    next
  }

  # fill = TRUE should not be necessary but is added for safety
  scores = lapply(score_files, \(x) {
    # We keep the Prediction and Resampling object in the individual files just in case,
    # but delete them from the combined object to speed things up significantly and save
    # on RAM. Throwing away these columns here saved me 4GB of RAM for "harrell_c,isbs"
    # for the scores DT. Same with warnign and error messages (we keep the count vars with _cnt suffix)
    this_score = readRDS(x)
    this_score[, resampling := NULL]
    this_score[, prediction_test := NULL]
    this_score[, errors := NULL]
    this_score[, warnings := NULL]
    this_score
  }) |>
    data.table::rbindlist(fill = TRUE)

  save_obj(scores, name = "scores_combined", suffix = tune_measure)
  tictoc::toc()

  tictoc::tic("aggrs")
  aggr_files = fs::path(conf$result_path, glue::glue("aggr_{tune_measure}_{current_learners}.rds"))
  aggr_files = mlr3misc::keep(aggr_files, fs::file_exists)

  aggr = lapply(aggr_files, \(x) {
    # Same idea as above - resample_result contains Prediction objects, roughly 1GB
    # in one case checked via pryr::object_size.
    this_aggr = readRDS(x)
    this_aggr[, resample_result := NULL]
    this_aggr
  }) |>
    data.table::rbindlist(fill = TRUE)

  save_obj(aggr, name = "aggr_combined", suffix = tune_measure)
  tictoc::toc()
}

# Combining for all tuning measures
aggr = fs::path(conf$result_path, glue::glue("aggr_combined_{tune_measures}.rds")) |>
  purrr::keep(fs::file_exists) |>
  lapply(readRDS) |>
  data.table::rbindlist(fill = TRUE) |>
  add_learner_groups()

scores = fs::path(conf$result_path, glue::glue("scores_combined_{tune_measures}.rds")) |>
  purrr::keep(fs::file_exists) |>
  lapply(readRDS) |>
  data.table::rbindlist(fill = TRUE) |>
  add_learner_groups()

save_obj(aggr, "aggr")
save_obj(scores, "scores")

# Creating bma objects --------------------------------------------------------------
# bmas can be created from aggr tables and are lightweight, but useful for mlr3benchmark functionality
# We need to get the aggrs for the untuned learners with tune_measure == "harrell_c,isbs"
# and add them to the variants for each tuning measures, hence the grep'ing

cols_harrell_c = c("task_id", "learner_id", msr_tbl[type == "Discrimination", id])
cols_isbs = c("task_id", "learner_id", msr_tbl[type != "Discrimination", id])

bma_harrell_c = aggr[grepl(pattern = "harrell_c", x = tune_measure), cols_harrell_c, with = FALSE]
bma_harrell_c[, task_id := factor(task_id)]
bma_harrell_c[, learner_id := factor(learner_id)]
bma_harrell_c = mlr3benchmark::as_benchmark_aggr(bma_harrell_c)

bma_isbs = aggr[grepl(pattern = "isbs", x = tune_measure), cols_isbs, with = FALSE]
bma_isbs[, task_id := factor(task_id)]
bma_isbs[, learner_id := factor(learner_id)]
bma_isbs = mlr3benchmark::as_benchmark_aggr(bma_isbs)

save_obj(bma_harrell_c, "bma", suffix = "harrell_c")
save_obj(bma_isbs, "bma", suffix = "isbs")

# Reassembling tuning archives ----------------------------------------------------------------
cli::cli_h2("Processing tuning archives")

# Archives with logs are very large objects of questionable utility, version without logs should suffice
if (!fs::file_exists(fs::path(conf$result_path, "archives.rds"))) {
  cli::cli_alert_info("Reassembling tuning archives including logs")
  tictoc::tic()
  archives = reassemble_archives(conf, keep_logs = TRUE)
  # Note ressamble_archives() writes archives.rds to result dir already
  tictoc::toc()
}

# if (!fs::file_exists(fs::path(conf$result_path, "archives_without-logs.rds"))) {
#   cli::cli_alert_info("Reassembling tuning archives without logs")
#   tictoc::tic()
#   archives_without_logs = reassemble_archives(conf, keep_logs = FALSE)
#   tictoc::toc()
# }

# Create zipped collection of tuning archive CSVs
if (!fs::file_exists(fs::path(conf$result_path, "archives.zip"))) {
  tictoc::tic("Zipping archives")
  convert_archives_csv(conf)

  zip_out = fs::path(conf$result_path, "archives.zip")

  utils::zip(
    zipfile = zip_out,
    files = fs::dir_ls(fs::path(conf$result_path, "tuning_archives")),
    flags = "-rD9j"
  )

  tictoc::toc()
}

cli::cli_alert_success("Done!")
tictoc::toc()

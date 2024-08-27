cli::cli_alert_info("Loading helpers.R")

library(data.table)
# Helpers run pre-benchmark -------------------------------------------------------------------

#' Store instantiated resamplings as portable CSV files to `here::here("resamplings"`)
#'
#' @param resampling Object of class `Resampling`, has to be instantiated.
#' @param task (`mlr3proba::TaskSurv`) Task object where `$id` will correspond to
#'   `<resampling_dir>/<task_id>.csv` file.
#' @param resampling_dir (`here::here("resamplings")`) Path of folder containing resampling CSVs.
save_resampling = function(resampling, task, resampling_dir = here::here("resamplings")) {
  ensure_directory(resampling_dir)
  mlr3::assert_resampling(resampling, instantiated = TRUE)
  mlr3::assert_task(task, task_type = "surv")
  stopifnot(resampling$is_instantiated)

  file_csv <- fs::path(resampling_dir, task$id, ext = "csv")
  write.csv(resampling$instance, file_csv, row.names = FALSE)
}

#' Reconstruct a Resampling object from stored resampling CSV
#'
#' Using tasks-specific resampling stored at `resampling_dir`.
#' @param task (`mlr3proba::TaskSurv`) Task object where `$id` is expected to correspond to
#'   `<resampling_dir>/<task_id>.csv` file.
#' @param resampling_dir (`here::here("resamplings")`) Path of folder containing resampling CSVs.
#'
#' @return Object of class `mlr3::ResamplingCustomCV` reconstructing the stored resampling folds.
#'
#' @examples
#' data = readRDS("datasets/cost.rds")
#' task = as_task_surv(data, target = "time", event = "status", id = "cost")
#' task$set_col_roles("status", add_to = "stratum")
#'
#' create_resampling_from_csv(task)
create_resampling_from_csv = function(task, resampling_dir = here::here("resamplings")) {
  mlr3::assert_task(task, task_type = "surv")

  resampling_csv_path = fs::path(resampling_dir, task$id, ext = "csv")
  checkmate::assert_file_exists(resampling_csv_path)

  # Read stored resampling, sort by row_id for easier assignment of folds in row_id order
  resampling_csv = as.data.table(read.csv(resampling_csv_path))
  resampling_csv = resampling_csv[order(resampling_csv$row_id), ]

  # Create new custom CV, using stored folds
  custom_cv = rsmp("custom_cv")
  folds = resampling_csv$fold[order(resampling_csv$row_id)]
  custom_cv$instantiate(task, f = factor(folds))

  # Sanity check that custom CV is identical to stored resampling
  resampling_reconstructed = data.table::rbindlist(lapply(names(custom_cv$instance), \(i) {
    data.frame(row_id = custom_cv$instance[[i]], fold = as.integer(i))
  }))
  resampling_reconstructed = resampling_reconstructed[order(resampling_reconstructed$row_id), ]

  stopifnot(all.equal(resampling_csv, resampling_reconstructed))

  custom_cv
}


#' Store additional data fro tasks
#'
#' Useful to augment job table and later results with e.g. n, p, number of unique event times, ...
#' Repeatedly refreshed in case there are changes with the included tasks in the benchmark.
#' Written to CSV such that possibly unintended changes are immediately obvious via `git status`.
#'
#' @param tasks List of `TaskSurv` objects.
#' @param path Path to store CSV file of results.
#' @examples
#'
#' tasks = load_task_data()
#' save_tasktab(tasks)
save_tasktab = function(tasks, path = here::here("attic", "tasktab.csv")) {

  names = names(tasks)

  # Save overview of tasks with some metadata which comes in handy later
  tasktab = data.table::rbindlist(lapply(seq_along(tasks), \(x) {
    task_data = tasks[[x]]

    if (!(inherits(task_data, "TaskSurv"))) {
      task = as_task_surv(task_data, target = "time", event = "status", id = names[x])
      task$set_col_roles("status", add_to = "stratum")
    } else {
      task = task_data
    }

    data.table::data.table(
      task_id = task$id,
      n = task$nrow,
      p = length(task$feature_names),
      dim = task$nrow * length(task$feature_names),
      n_uniq_t = length(unique(task$data(cols = "time")[[1]])),
      events = sum(task$data(cols = "status")[[1]] == 1),
      censprop = round(mean(task$data(cols = "status")[[1]] == 0), 4)
    )
  }))
  tasktab[, dimrank := data.table::frank(dim)]
  tasktab[, uniq_t_rank := data.table::frank(n_uniq_t)]

  write.csv(tasktab, path, row.names = FALSE)
  tasktab
}

#' Load tasks as they were before resampling
#'
#' To just get the tasks as a named list of `data.table` objects
load_task_data = function() {
  files = dir(here::here("datasets"), pattern = "\\.rds$", full.names = TRUE)
  names = stringi::stri_sub(basename(files), 1, -5)
  tasks = mlr3misc::named_list(names)

  for (i in seq_along(files)) {
    tasks[[i]] = readRDS(files[i])
  }

  tasks
}

#' mlr3tuning callback to save the tuning archive outside of the job.
#'
#' Avoid having to serialize the entire tuning instance, i.e. `store_tuning_instance = FALSE` can
#' be used to reduce the result serialization overhead.
#'
#' The file path for storage is derived from the registry directory, learner, task and resampling hash.
#' E.g.: `/path/to/reg_dir/tuning_archives/tuning.measure__learner.id__task.id__unix.epoch__hash(task$data()).rds`
callback_backup_impl = function(callback, context) {

  # Ensure the folder containing tuning archives exists
  ensure_directory(callback$state$path_dir)

  task_id = context$instance$objective$task$id
  # We don't have the outer resampling iter number so hashing
  # the data is the next best thing we can do
  iter_hash = digest::digest(context$instance$objective$task$data())

  # Construct a file name that is hopefully fully unambiguous
  callback$state$file_name = sprintf(
    "%s__%s__%s__%i__%s.rds",
    callback$state$tuning_measure,
    callback$state$learner_id,
    task_id,
    as.integer(Sys.time()), # unix epoch for good measure and sortability
    iter_hash
  )
  # Assemble path based on directory and filename, store in state just in case.
  callback$state$path = fs::path(callback$state$path_dir, callback$state$file_name)

  # cli::cli_alert_info("Writing archive to {callback$state$path}")
  saveRDS(data.table::as.data.table(context$instance$archive), callback$state$path)
}

#' Callback to find the learner logs and append them to the tuning archive.
#' Archives get serialized to disk and can later be recovered to find
#' error messages which would otherwise be obscured by using the fallback learners.
callback_archive_logs_impl = function(callback, context) {

  states = context$benchmark_result$.__enclos_env__$private$.data$learners(states = TRUE)

  logs = data.table::rbindlist(lapply(states$learner, \(x) {
    x$state$log
  }))

  # Only attach log if there's something to attach
  # if (any(sapply(logs$learner_log, nrow) > 0)) {
  context$aggregated_performance[, log := list(logs)]
  # }

}

# Utilities for analysis ----------------------------------------------------------------------

#' List of included learners with short IDs and some extra info
#'
#' @param path Path to store CSV file.
#'
#' @examples
#' save_lrntab()
save_lrntab <- function(path = here::here("attic", "learners.csv")) {
  lrnlist <- list(
    KM = list(learner = "surv.kaplan", params = 0),
    NL = list(learner = "surv.nelson", params = 0),
    AK = list(learner = "surv.akritas", params = 1),
    CPH = list(learner = "surv.coxph", params = 0),
    GLMN = list(learner = "surv.cv_glmnet", .encode = TRUE, params = 1, internal_cv = TRUE),
    Pen = list(learner = "surv.penalized", params = 2),
    AFT = list(learner = "surv.parametric", params = 1, grid = TRUE),
    Flex = list(learner = "surv.flexible", params = 1, grid = TRUE),
    RFSRC = list(learner = "surv.rfsrc", params = 5),
    RAN = list(learner = "surv.ranger", params = 5),
    CIF = list(learner = "surv.cforest", params = 5),
    ORSF = list(learner = "surv.aorsf", params = 2),
    RRT = list(learner = "surv.rpart", params = 1, grid = TRUE),
    MBST = list(learner = "surv.mboost", params = 4),
    CoxB = list(learner = "surv.cv_coxboost", .encode = TRUE, params = 0, internal_cv = TRUE),
    XGBCox = list(learner = "surv.xgboost.cox", .encode = TRUE, params = 6, .form = "ph"),
    XGBAFT = list(learner = "surv.xgboost.aft", .encode = TRUE, params = 8, .form = "aft"),
    SSVM = list(learner = "surv.svm", .encode = TRUE, .scale = TRUE, params = 4)
  ) |>
    lapply(data.table::as.data.table) |>
    data.table::rbindlist(fill = TRUE, idcol = TRUE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(is.na(.x), FALSE, .x))) |>
    # setNames(c("learner_id", "learner_id_long", "params", "encode", "internal_cv", "grid", "scale")) |>
    dplyr::select(learner_id = .id, learner_id_long = learner, params, internal_cv, encode = .encode, scale = .scale, grid)


  lrnlist |>
    write.csv(file = path, row.names = FALSE)

  lrnlist
}

load_lrntab = function(path = here::here("attic", "learners.csv")) {
  read.csv(path)
}

load_tasktab = function(path = here::here("attic", "tasktab.csv")) {
  read.csv(path)
}

#' Quickly recreate list of evaluation measures
#'
#' Define once, use everywhere.
get_measures_eval = function() {
  measures_eval = list(
    msr("surv.cindex",      id = "harrell_c",                      label = "Harrell's C"),
    msr("surv.cindex",      id = "uno_c",      weight_meth = "G2", label = "Uno's C"),

    # msr("surv.rcll",        id = "rcll",     ERV = FALSE,  label = "Right-Censored Log Loss (RCLL)"),
    # msr("surv.rcll",        id = "rcll_erv", ERV = TRUE,   label = "Right-Censored Log Loss (RCLL) [ERV]"),

    # Default has IPCW = FALSE, resulting in RNLL (proper) rather than NLL according to proba docs
    msr("surv.logloss",     id = "rnll",     ERV = FALSE, label = "Re-weighted Negative Log-Likelihood (RNLL)"),
    msr("surv.logloss",     id = "rnll_erv", ERV = TRUE,  label = "Re-weighted Negative Log-Likelihood (RNLL) [ERV]"),

    msr("surv.intlogloss",  id = "risll",     ERV = FALSE, proper = TRUE, label = "Re-weighted Integrated Survival Log-Likelihood (RISLL)"),
    msr("surv.intlogloss",  id = "risll_erv", ERV = TRUE,  proper = TRUE, label = "Re-weighted Integrated Survival Log-Likelihood (RISLL) [ERV]"),

    msr("surv.brier",        id = "isbs",     proper = FALSE,  ERV = FALSE, label = "Integrated Survival Brier Score (ISBS)"),
    msr("surv.brier",        id = "isbs_erv", proper = FALSE,  ERV = TRUE,  label = "Integrated Survival Brier Score (ISBS) [ERV]"),

    # Unsued, kept for completeness
    # msr("surv.brier",        id = "risbs",     proper = TRUE,   ERV = FALSE, label = "Re-weighted Integrated Survival Brier Score (RISBS)"),
    # msr("surv.brier",        id = "risbs_erv", proper = TRUE,   ERV = TRUE,  label = "Re-weighted Integrated Survival Brier Score (RISBS) [ERV]"),

    msr("surv.dcalib",      id = "dcalib", truncate = 1000, label = "D-Calibration"),

    msr("surv.calib_alpha", id = "caliba_ratio", method = "ratio", truncate = 1000, label = "Van Houwelingen's Alpha"),
    # Unsued, kept for completeness
    # msr("surv.calib_alpha", id = "caliba_diff",  method = "diff",  label = "Van Houwelingen's Alpha [Difference Method]")

  )
  names(measures_eval) = mlr3misc::ids(measures_eval)
  measures_eval
}

#' Table of evaluation measures with metadata
#'
measures_tbl = function() {
  msr_tbl = mlr3misc::rowwise_table(
    ~mlr_id,            ~id,           ~type,
    "surv.cindex",      "harrell_c",    "Discrimination",
    "surv.cindex",      "uno_c",        "Discrimination",

    # "surv.rcll",        "rcll",         "Scoring Rule",
    # "surv.rcll",        "rcll_erv",     "Scoring Rule",

    "surv.risll",       "risll",        "Scoring Rule",
    "surv.risll",       "risll_erv",    "Scoring Rule",

    "surv.logloss",     "rnll",         "Scoring Rule",
    "surv.logloss",     "rnll_erv",     "Scoring Rule",

    "surv.brier",       "isbs",         "Scoring Rule",
    "surv.brier",       "isbs_erv",     "Scoring Rule",

    # Unsued, kept for completeness
    # "surv.brier",       "risbs",        "Scoring Rule",
    # "surv.brier",       "risbs_erv",    "Scoring Rule",

    "surv.dcalib",      "dcalib",       "Calibration",
    "surv.calib_alpha", "caliba_ratio", "Calibration",
    # Unused, kept for completeness
    "surv.calib_alpha", "caliba_diff",  "Calibration"
    # "surv.calib_alpha", "caliba",
  )

  measures_eval = get_measures_eval()
  msr_tbl = data.table(
    measure = measures_eval,
    id = mlr3misc::ids(measures_eval)
  )[msr_tbl, on = "id"]

  msr_tbl[, label := vapply(msr_tbl$measure, \(x) x$label, "", USE.NAMES = FALSE)]
  # ERV is either a logical param or not present --> implies ERV = FALSE
  msr_tbl[, erv := vapply(measure, \(x) isTRUE(x$param_set$values$ERV), logical(1), USE.NAMES = FALSE)]
  # minimize argument should be FALSE if measure is ERV or C-index (suffixes _erv or _c)
  # msr_tbl[, erv := grepl("_erv$", id)]
  # msr_tbl[, minimize := !(erv | grepl("_c$", id))]

  # Minimize is taken from measure-encoded value, but overridden if ERV = TRUE as ERV can't be minimized
  msr_tbl[, minimize := vapply(measure, \(x) x$minimize, logical(1), USE.NAMES = FALSE)]
  msr_tbl[, minimize := fifelse(erv, FALSE, minimize)]

  msr_tbl[]
}



#' Collect and save benchmark results
#'
#' @param settings `config::get()` for registry names, paths, ...
#' @param tuning_measure E.g. "harrell_c"
#' @param measures_eval List of mlr3 measures used for evaluation
#' @param result_path `here::here("results")`, where to store results.
#'   A subfolder based on registry folder name will be created.
#'
#' @return Nothing
#'
#' @examples
collect_results = function(
    settings,
    tuning_measure = "harrell_c",
    measures_eval = get_measures_eval(),
    include_scores = FALSE,
    id_filter = NULL
) {

  tictoc::tic("The whole shebang")

  reg = suppressWarnings(suppressMessages(batchtools::loadRegistry(
    settings$reg_dir, writeable = TRUE
  )))

  cli::cli_alert_info("Using registry {.file {settings$reg_name}}")
  cli::cli_alert_info("Processing results ({tuning_measure}) in {fs::path_rel(settings$result_path)}")
  ensure_directory(settings$result_path)

  selected_ids = batchtools::findTagged(tuning_measure, reg = reg)
  done_ids = batchtools::findDone(selected_ids)
  done_perc = round(100 * nrow(done_ids)/nrow(selected_ids), 3)
  cli::cli_alert_info("Found {nrow(selected_ids)} job ids of which {nrow(done_ids)} are done ({done_perc}%)")

  if (!is.null(id_filter)) {
    done_ids = ijoin(done_ids, id_filter)
    cli::cli_alert_info("Filtering down to {nrow(done_ids)} ids")
  }

  path_bmr     = glue::glue("{settings$result_path}/bmr_{tuning_measure}.rds")
  path_bmr_tab = glue::glue("{settings$result_path}/bmrtab_{tuning_measure}.rds")
  path_bma     = glue::glue("{settings$result_path}/bma_{tuning_measure}.rds")
  path_scores  = glue::glue("{settings$result_path}/scores_{tuning_measure}.rds")

  if (all(fs::file_exists(c(path_bmr, path_bma, path_bmr_tab))) & (fs::file_exists(path_scores) | !include_scores)) {
    return(cli::cli_alert_success("bmr, bma and aggr already exist!"))
  }

  if (!file.exists(path_bmr)) {
    tictoc::tic(msg = glue::glue("Reducing results ({tuning_measure})"))
    bmr = mlr3batchmark::reduceResultsBatchmark(
      ids = done_ids,
      store_backends = TRUE,
      reg = reg
    )
    tictoc::toc()

    gc()

    bmr_tab = bmr$aggregate(measures = list(), conditions = TRUE)
    bmr_tab = data.table::as.data.table(bmr_tab)
    bmr_tab[, resample_result := NULL]
    saveRDS(bmr_tab, path_bmr_tab)

    # benchmark result
    tictoc::tic(msg = glue::glue("Saving bmr ({tuning_measure})"))
    saveRDS(bmr, file = path_bmr)
    tictoc::toc()

  } else if (!fs::file_exists(path_bma)) {

    cli::cli_alert_info("Reading bmr from disk ({tuning_measure})")
    tictoc::tic(msg = glue::glue("Reading bmr"))
    bmr = readRDS(path_bmr)
    tictoc::toc()

  }

  gc()

  # bma via mlr3benchmark
  if (!fs::file_exists(path_bma)) {
    cli::cli_alert_info("as_benchmark_aggr'ing results ({tuning_measure})")
    tictoc::tic(msg = "as_benchmark_aggr'ing")
    bma = mlr3benchmark::as_benchmark_aggr(bmr, measures = measures_eval)
    tictoc::toc()

    cli::cli_alert_info("Saving bma ({tuning_measure})")
    saveRDS(bma, path_bma)
  } else {
    cli::cli_alert_success("bma already saved!")
  }

  tictoc::toc()
}

#' Score bmr with a measure and store results
#'
#' While collect_results() creates the bmr and bma,
#' it's probably useful to separately store aggregates and scores
#' which makes it easier to re-score with certain measures without
#' having to apply all measures again.
#'
#' @param settings `config::get()`
#' @param tuning_measure (`character(1)`, `"harrell_c"`) The tuning measure used to filter the `bmr`
#' @param measure One or a list of `MeasureSurv`s
#' @param nthreads (`1`) Parallelize using forking with `plan("multicore")`.
#'   **Does not work on Windows or in RStudio**

#' @examples
#' score_bmr(config::get(), "harrell_c",  msr("surv.isbs"))
score_bmr = function(
    settings = config::get(),
    tuning_measure = "harrell_c",
    measure,
    nthreads = 1
) {

  if (!is.list(measure)) measure = list(measure)
  sapply(measure, checkmate::assert_class, "MeasureSurv")
  checkmate::assert_int(nthreads, lower = 1, upper = future::availableCores())

  path_bmr = fs::path(settings$result_path, glue::glue("bmr_{tuning_measure}.rds"))
  checkmate::assert_file_exists(path_bmr)

  cli::cli_alert_info("Reading bmr ({tuning_measure})")
  bmr = readRDS(path_bmr)

  if (nthreads > 1 & length(measure) > 1) {
    future::plan("multicore", workers = nthreads)
  } else {
    future::plan("sequential")
  }

  pb = cli::cli_progress_bar("Scoring", total = length(measure))
  future.apply::future_lapply(measure, \(m) {
    cli::cli_progress_update(id = pb)
    path_scores = fs::path(settings$result_path, tuning_measure, "scores", glue::glue("scores_{m$id}.rds"))
    ensure_directory(fs::path_dir(path_scores))

    if (fs::file_exists(path_scores)) {
      cli::cli_alert_warning("{fs::path_file(path_scores)} already exists!")
      return()
    }

    cli::cli_alert_info("$score'ing results with {m$id} ({tuning_measure})")
    tictoc::tic(msg ="$score'ing")
    scores = bmr$score(measures = m, conditions = TRUE)
    tictoc::toc()

    # Trimming some fat
    scores = data.table::setDT(scores)
    scores = mlr3misc::remove_named(
      scores, c("task", "resampling", "learner", "prediction")
    )
    saveRDS(scores, path_scores)
  }, future.seed = NULL)

  cli::cli_progress_done(id = pb)
}

#' Aggregate bmr with a measure and store results
#'
#' @param settings `config::get()`
#' @param tuning_measure
#' @param measure One or a list of `MeasureSurv`s
#'
#' @return
#' @export
#'
#' @examples
#' aggr_bmr(config::get(), "harrell_c", msr("surv.isbs"))
aggr_bmr = function(
    settings = config::get(),
    tuning_measure = "harrell_c",
    measure,
    nthreads = 1
) {

  if (!is.list(measure)) measure = list(measure)
  sapply(measure, checkmate::assert_class, "MeasureSurv")

  path_bmr = fs::path(settings$result_path, glue::glue("bmr_{tuning_measure}.rds"))
  checkmate::assert_file_exists(path_bmr)

  cli::cli_alert_info("Reading bmr ({tuning_measure})")
  bmr = readRDS(path_bmr)

  if (nthreads > 1 & length(measure) > 1) {
    future::plan("multicore", workers = nthreads)
  } else {
    future::plan("sequential")
  }

  pb = cli::cli_progress_bar("Scoring", total = length(measure))
  future.apply::future_lapply(measure, \(m) {
    cli::cli_progress_update(id = pb)
    path_aggr = fs::path(settings$result_path, tuning_measure, "aggr", glue::glue("aggr_{m$id}.rds"))
    ensure_directory(fs::path_dir(path_aggr))

    if (fs::file_exists(path_aggr)) {
      cli::cli_alert_warning("{fs::path_file(path_aggr)} already exists!")
      return()
    }

    cli::cli_alert_info("$aggregate'ing results with {m$id} ({tuning_measure})")
    tictoc::tic(msg ="$aggregate'ing")
    aggr = bmr$aggregate(measures = m, conditions = TRUE)
    tictoc::toc()

    # Trimming some fat
    aggr = data.table::setDT(aggr)
    aggr = mlr3misc::remove_named(aggr, c("resample_result"))
    saveRDS(aggr, path_aggr)
  }, future.seed = NULL)

  cli::cli_progress_done(id = pb)
}

#' Collect tuning archives saved separately to disk via callback
#' @param settings `config::get()` for result paths
#' @param keep_logs [`TRUE`] Whether to keep the logs, which drastically increases the size
#'   of the resulting object in memory and on disk
#' @param ignore_cache [`FALSE`] Whether to ignore a cached result and reassemble from scratch
reassemble_archives = function(
    settings = config::get(),
    keep_logs = TRUE,
    ignore_cache = FALSE
  ) {

  if (keep_logs) {
    archive_path = fs::path(settings$result_path, "archives-with-logs.rds")
  } else {
    archive_path = fs::path(settings$result_path, "archives-no-logs.rds")
  }

  ensure_directory(fs::path_dir(archive_path))

  if (fs::file_exists(archive_path)) {
    if (!ignore_cache) {
      cli::cli_alert_info("Archives already aggregated, returning cache from {.file {fs::path_rel(archive_path)}}")
      return(readRDS(archive_path))
    } else {
      cli::cli_alert_info("Ignoring that {.file {fs::path_rel(archive_path)}} is already present, overwriting...")
    }
  }

  archive_dir = fs::path(settings$reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)

  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {.file {fs::path_rel(archive_dir)}}!")
  }

  learners = load_lrntab()
  learners = learners[, c("learner_id", "learner_id_long")]

  # learners$learner_id_long = dplyr::case_when(
  #   learners$learner_id == "XGBCox" ~ "surv.xgboostcox",
  #   learners$learner_id == "XGBAFT" ~ "surv.xgboostaft",
  #   .default = learners$learner_id_long
  # )

  pb = cli::cli_progress_bar("Reading tuning archives", total = length(tuning_files))
  archives = data.table::rbindlist(lapply(tuning_files, \(file) {
    cli::cli_progress_update(id = pb)
    archive = readRDS(file)

    if (!keep_logs) {
      # Temp fix because objects became to large
      archive[, log := NULL]
    } else {
      # Including the fallback log at all was a mistake
      mlr3misc::walk(archive$log, \(log) {
        mlr3misc::remove_named(log, "fallback_log")
      })
    }

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")
    components = components[[1]]
    names(components) = c("tune_measure", "learner_id_long", "task_id", "time_epoch", "iter_hash")

    ret = data.table::data.table(
      t(components),
      archive = list(archive),
      file = file,
      warnings_sum = sum(archive$warnings),
      errors_sum = sum(archive$errors)
    )
  }))
  cli::cli_progress_done(id = pb)

  archives = archives[learners, on = "learner_id_long"]
  archives = archives[!is.na(tune_measure), ]
  archives[, learner_id_long := NULL]

  saveRDS(archives, archive_path)

  archives[]
}

#' Read tuning archives stored in registry and convert to CSV in results dir
#' @param settings `config::get()` for result paths.
#'
#' @return Nothing, only writes files.
convert_archives_csv = function(settings = config::get()) {

  archive_csv_dir = fs::path(settings$result_path, "tuning_archives")
  ensure_directory(archive_csv_dir)

  archive_dir = fs::path(settings$reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)

  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {.file {fs::path_rel(archive_dir)}}!")
  }

  # Having to fix XGBoost split not corresponding to learner IDs at time of benchmark
  learners = load_lrntab()
  learners = learners[, c("learner_id", "learner_id_long")]

  # learners$learner_id_long = dplyr::case_when(
  #   learners$learner_id == "XGBCox" ~ "surv.xgboostcox",
  #   learners$learner_id == "XGBAFT" ~ "surv.xgboostaft",
  #   .default = learners$learner_id_long
  # )

  pb = cli::cli_progress_bar("Reading tuning archives", total = length(tuning_files))
  purrr::walk(tuning_files, \(file) {
    cli::cli_progress_update(id = pb)
    archive = readRDS(file)

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")

    components = components[[1]]
    names(components) = c("tune_measure", "learner_id_long", "task_id", "time_epoch", "iter_hash")
    components = components[c("tune_measure", "learner_id_long", "task_id", "iter_hash")]

    to_csv = cbind(
      data.table::data.table(t(components[c("tune_measure", "learner_id_long", "task_id", "iter_hash")])),
      archive
    )

    out_path = fs::path(archive_csv_dir, paste(components, collapse = "__"), ext = "csv")
    if (!fs::file_exists(out_path)) {

      # cli::cli_alert_info("Writing archive for {.val {components[['learner_id_long']]}} tuned with \\
      #                   {.val {components[['tune_measure']]}} on {.val {components[['task_id']]}} \\
      #                   (iter {.val {components[['iter_hash']]}}) to CSV")

      readr::write_csv(x = to_csv, file = out_path, append = FALSE)
    }

  })
  cli::cli_progress_done(id = pb)
}


#' Removing duplicate tuning archives
#'
#' When jobs are resubmitted, the previous tuning archive is not removed automatically,
#' so the associated timestamp is used to identify the older archive and move it to
#' a backup location.
#' @param settings `config::get()`
#' @param tmp_path `here::here("tmp", "archive-backup")`.
#'
#' @examples
#' clean_duplicate_archives(config::get())
clean_duplicate_archives = function(
  settings,
  tmp_path = here::here("tmp", "archive-backup")
) {

  ensure_directory(tmp_path)

  archives = reassemble_archives(settings, keep_logs = FALSE)

  counts = archives[, .(n = .N), by = .(tune_measure, task_id, learner_id, iter_hash)]

  archives = archives[counts, on = .(tune_measure, task_id, learner_id, iter_hash)]
  archives = archives[n > 1, ]

  if (nrow(archives) > 0) {
    dupes = archives |>
      dplyr::group_by(tune_measure, task_id, learner_id, iter_hash) |>
      dplyr::filter(time_epoch == min(time_epoch))

    cli::cli_alert_info("There are {nrow(dupes)} duplicates")
    cli::cli_alert_danger("Moving files to {.path fs::path_rel(tmp_path)}")

    fs::file_move(dupes$file, tmp_path)
  } else {
    cli::cli_alert_info("Found no duplicate archives")
  }

  cli::cli_alert_success("Done!")
}

#' Check if scores are valid / invalid
#'
#' Validity in this case only meaning it's neither Inf, NA, nor NaN
#' @param x `numeric()`
is_valid = function(x) {
  is.finite(x) & !is.na(x) & !is.nan(x)
}
is_invalid = function(x) !is_valid(x)

check_scores = function(bma) {
  xdat = bma$data

  res = lapply(names(xdat), \(x) {
    tibble::tibble(
      measure = x,
      NA_n = sum(is.na(xdat[[x]]) & !is.nan(xdat[[x]])),
      Inf_n = sum(is.infinite(xdat[[x]])),
      NaN_n = sum(is.nan(xdat[[x]])),
      total = NA_n + Inf_n + NaN_n
    )
  }) |>
    data.table::rbindlist(use.names = TRUE)

  res[total > 0, ]
}

#' Exclude results for given learner or task IDs in case of kerfuffle.
#' @param x A `bma`/`BemcharkAggr` or `data.frame`-like object with appropriate column names.
#' @param learner_id_exclude (`character()`) Learner IDs to exclude.
#' @param task_id_exclude (`character()`) Task IDs to exclude.
remove_results = function(x,
                          learner_id_exclude = NULL,
                          task_id_exclude = NULL
) {

  if (inherits(x, "BenchmarkAggr")) {
    checkmate::assert_class(x, classes = "BenchmarkAggr")
    xdat = data.table::copy(x$data)
  } else if (inherits(x, "data.frame")) {
    xdat = checkmate::assert_data_table(x)
  } else {
    stop("Expecting a BenchmarkAggr or data.frame-like object")
  }

  if (!is.null(learner_id_exclude)) {
    learner_id_orig = unique(as.character(xdat[["learner_id"]]))
    checkmate::assert_subset(learner_id_exclude, learner_id_orig)

    xdat = xdat[xdat[["learner_id"]] != learner_id_exclude, ]
    xdat = xdat[, learner_id := factor(learner_id, levels = setdiff(learner_id_orig, learner_id_exclude))]
  }

  if (!is.null(task_id_exclude)) {
    task_id_orig = unique(as.character(xdat[["task_id"]]))
    checkmate::assert_subset(task_id_exclude, task_id_orig)

    xdat = xdat[xdat[["task_id"]] != task_id_exclude, ]
    xdat = xdat[, task_id := factor(task_id, levels = setdiff(task_id_orig, task_id_exclude))]
  }



  if (inherits(x, "BenchmarkAggr")) return(mlr3benchmark::as_benchmark_aggr(xdat))
  xdat[]
}


#' When experiments were started less thought was put into the abbreviations we ended
#' up using for the results etc. plots so this is the "fixing stuff up" function.
#' @param x A `bma`/`BemcharkAggr` or `data.frame`-like object with appropriate column names.
rename_learners = function(x) {
  if (inherits(x, "BenchmarkAggr")) {
    checkmate::assert_class(x, classes = "BenchmarkAggr")
    xdat = data.table::copy(x$data)
  } else if (inherits(x, "data.frame")) {
    xdat = checkmate::assert_data_table(x)
  } else (
    stop("Expecting a BenchmarkAggr or data.frame-like object")
  )

  xdat[, learner_id := dplyr::case_when(
    # learner_id == "AF" ~ "AK",    # Akritas
    # learner_id == "NL" ~ "NA",    # Nelson-Aalen
    learner_id == "Par" ~ "AFT",  # AFT more standard than "parametric"
    # learner_id == "MBO" ~ "MBST", # mboost
    # learner_id == "GLM" ~ "GLMN", # glmnet
    TRUE ~ learner_id
  )]

  learner_order = c("KM", "NA", "AK", "CPH", "GLMN", "Pen", "AFT", "Flex", "RFSRC",
                    "RAN", "CIF", "ORSF", "RRT", "MBST", "CoxB", "XGBCox", "XGBAFT")

  xdat[, learner_id := factor(learner_id, levels = learner_order)]

  if (inherits(x, "BenchmarkAggr")) return(mlr3benchmark::as_benchmark_aggr(xdat))
  xdat[]
}

combine_bma = function(bma_harrell_c, bma_isbs) {
  checkmate::assert_class(bma_harrell_c, classes = "BenchmarkAggr")
  checkmate::assert_class(bma_isbs, classes = "BenchmarkAggr")

  bma1 = data.table::copy(bma_harrell_c$data)
  bma1[, tuned := "harrell_c"]

  bma2 = data.table::copy(bma_isbs$data)
  bma2[, tuned := "isbs"]

  data.table::rbindlist(list(bma1, bma2))
}

add_learner_groups = function(x) {
  if (checkmate::test_class(x, classes = "BenchmarkAggr")) {
    x = data.table::copy(x$data)
  } else {
    checkmate::assert_data_table(x)
  }

  x |>
    dplyr::mutate(
      learner_group = dplyr::case_match(
        learner_id,
        c("KM", "NA", "AK") ~ "Baseline",
        c("CPH", "GLMN", "Pen", "AFT", "Flex") ~ "Classical",
        c("RRT", "RFSRC", "RAN", "CIF", "ORSF") ~ "Trees",
        c("MBST", "XGBCox", "XGBAFT", "CoxB") ~ "Boosting",
        .ptype = factor(levels = c("Baseline", "Classical", "Trees", "Boosting"))
      )
    )
}

#' Collect individually `$score()`'d or `$aggregate()`'d results from `settings$result_path`.
#' @param settings `list()` of settings, `config::get()`
#' @param tuning_measure `character(1)`, one of `"harrell_c"` or `"isbs"`
#' @param type `character(1)`, one of `"scores"` or `"aggr"`
combine_scores_aggrs = function(
    settings = config::get(),
    tuning_measure = "harrell_c",
    type = "scores"
) {

  checkmate::assert_subset(tuning_measure, choices = c("harrell_c", "isbs"))
  checkmate::assert_subset(type, choices = c("scores", "aggr"))

  # Load all scores from files
  files = fs::dir_ls(fs::path(settings$result_path, tuning_measure, type))
  dts = lapply(files, readRDS)
  dts

  if (type == "scores") {
    base_cols = c("uhash", "nr", "task_id", "learner_id", "resampling_id",
                  "iteration", "warnings", "errors")
  }

  # Paranoid check that alle base columns are identical across all dts
  basecollist = unname(lapply(dts, \(x) x[, ..base_cols]))
  stopifnot(sapply(2:(length(dts) - 1), \(i) {
    identical(basecollist[[1]], basecollist[[i]])
  }))

  # Create base dt of only base columns for identificiation that's the same
  # across all dts and consecutively cbind score columns
  basedt = basecollist[[1]]
  for (dti in seq_along(dts)) {
    measure_col = setdiff(names(dts[[dti]]), base_cols)
    basedt = cbind(basedt, dts[[dti]][, ..measure_col])
  }

  basedt[, tuned := ..tuning_measure][]
}




tablify = function(x, caption = NULL, ...) {
  x |>
    kableExtra::kbl(caption = caption, booktabs = TRUE, ...) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
}

#' BenchmarkAggr plot wrapper
#'
#' Unified interface for various autoplot.BenchmarkAggr types.
#'
#' @param bma (`BenchmarkAggr`)
#' @param type One of `"mean", "box", "fn"` or `"cd_n"` for Critical Differences (Nemenyi) or
#'   `"cd_bd"` for Critical Differences with baseline comparison
#' @param measure_id,tuning_measure_id (`character(1)`) Measure ids as per `measures_tbl()`.
#' @param flip (`FALSE`) If `TRUE`, applies `coord_flip()`.
#' @param dodge (`TRUE`) If `TRUE`, applies `scale_x_discrete(guide = guide_axis(n.dodge = 2))`.
#' @param ... Additional arguments passed to `autoplot()`.
#'
#' @return
#' @export
#'
#' @examples
#' plot_results(bma_harrell_c, type = "box")
#' plot_results(bma_harrell_c, type = "mean")
#' plot_results(bma_harrell_c, type = "fn")
#' plot_results(bma_harrell_c, type = "cd_n", ratio = 1)
plot_results = function(
    bma, type = "box", measure_id = "harrell_c", tuning_measure_id = "harrell_c",
    exclude_learners = "",
    flip = FALSE,
    dodge = TRUE,
    ...
) {

  checkmate::assert_subset(type, choices = c("mean", "box", "fn", "cd_n", "cd_bd"))
  measure_label = msr_tbl[id == measure_id, label]
  tuning_measure_label = msr_tbl[id == tuning_measure_id, label]

  if (type %in% c("box", "mean")) {
    if (inherits(bma, "BenchmarkAggr")) xdat = bma$data else xdat = bma
    xdat = bma$data
    bma = mlr3benchmark::as_benchmark_aggr(xdat[!(learner_id %in% exclude_learners), ])
  }

  plot_type_label = switch(
    type,
    mean = "Mean ± SE",
    box = "Boxplot",
    fn = "Post-hoc Friedman-Nemenyi",
    cd_n = "Critical Differences (Nemenyi)",
    cd_bd = "Critical Differences (Bonferroni-Dunn)"
  )

  minimize = msr_tbl[id == measure_id, minimize]
# browser()
  if (type %in% c("cd_n", "cd_bd")) {

    test = switch(type, cd_n = "nemenyi", cd_bd = "bd")

    #cli::cli_alert_info("Performing type {type} and measure {measure_id} and test {test}, minimize = {minimize}")

    p = mlr3viz::autoplot(bma, type = "cd", meas = measure_id, test = test, minimize = minimize, ...) +
      labs(
        caption = glue::glue("Evaluation measure: {measure_label}
                             Tuning measure: {tuning_measure_label}")
      )
  } else if (type %in% c("mean", "box", "fn")) {
    p = mlr3viz::autoplot(bma, type = type, meas = measure_id, ...)

    p = p +
      labs(
        title = measure_label,
        subtitle = if (type != "box") plot_type_label else NULL,
        x = NULL,
        y = measure_label,
        caption = glue::glue("Tuning measure: {tuning_measure_label}")
      )
  } else {
    mlr3misc::stopf("Unknown type %s", type)
  }

  if (type %in% c("box", "mean")) {
    if (dodge) p = p + scale_x_discrete(guide = guide_axis(n.dodge = 2))
    if (flip) {
      p = p + coord_flip() +
        scale_x_discrete(limits = rev)
    }
    p = p + theme_minimal(base_size = 15)
    p = p + theme(plot.background = element_rect(fill = "transparent", color = NA))
  }

  if (type == "fn") {
    p = p + theme(
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_text(angle = 0)
    )
  }

  p

}

#' Unfortunate partial duplication as above function is for BenchmarkAggr objects but turns out
#' I want one for "normal" data.frames as well. Should have seen that coming.
#'
plot_aggr_scores = function(xdf, type = "box", eval_measure_id = "harrell_c", tuning_measure_id = "harrell_c", dodge = FALSE, flip = FALSE) {
  checkmate::assert_data_table(xdf)

  measure_label = msr_tbl[id == eval_measure_id, label]
  tuning_measure_label = msr_tbl[id == tuning_measure_id, label]

  plot_type_label = switch(type, mean = "Mean ± SE", box = "Boxplot")

  minimize = msr_tbl[id == eval_measure_id, minimize]

  if (minimize) {
    direction_label = "lower is better"
  } else {
    direction_label = "higher is better"
  }

  p = ggplot(xdf[tuned == tuning_measure_id], aes(x = learner_id, y = .data[[eval_measure_id]], color = learner_group, fill = learner_group)) +
    geom_boxplot(alpha = 1/4, key_glyph = "rect") +
    scale_color_manual(values = palette_groups, aesthetics = c("color", "fill"), name = NULL)

  p = p +
    labs(
      title = measure_label,
      subtitle = glue::glue("{plot_type_label} of aggregated scores across all tasks ({direction_label})"),
      x = NULL,
      y = measure_label,
      color = NULL, fill = NULL,
      caption = glue::glue("Tuning measure: {tuning_measure_label}")
    )

  if (dodge) p = p + scale_x_discrete(guide = guide_axis(n.dodge = 2))
  if (flip) {
    p = p + coord_flip() +
      scale_x_discrete(limits = rev)
  }
  p = p + theme_minimal(base_size = 15)
  p = p + theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

  print(p)
}

#' Analogous plotting function but for by-dataset plots, slightly different, could be consolidated though
plot_scores = function(scores, eval_measure_id = "harrel_c", tuning_measure_id = "harrel_c", dodge = FALSE, flip = TRUE) {
  checkmate::assert_data_table(scores)
  checkmate::assert_subset(eval_measure_id, choices = msr_tbl$id)
  checkmate::assert_subset(tuning_measure_id, choices = c("isbs", "harrell_c"))

  if (msr_tbl[id == eval_measure_id, minimize]) {
    direction_label = "lower is better"
  } else {
    direction_label = "higher is better"
  }

  p = scores |>
    dplyr::filter(tuned == tuning_measure_id) |>
    ggplot(aes(y = learner_id, x = .data[[eval_measure_id]], color = learner_group, fill = learner_group)) +
    facet_wrap(vars(task_id), scales = "free_x", ncol = 8) +
    geom_boxplot(alpha = 1/4) +
    scale_color_manual(values = palette_groups, aesthetics = c("color", "fill")) +
    labs(
      title = msr_tbl[id == eval_measure_id, label],
      subtitle = glue::glue("Scores per dataset across outer resampling folds ({direction_label})"),
      x = "Score", y = NULL,
      color = NULL,
      fill = NULL,
      caption = glue::glue("Tuning measure: {msr_tbl[id == tuning_measure_id, label]}"),
    ) +
    theme_minimal(
      base_size = 11#,
      # Would want to use custom fonts but reproducibility... :(
      # base_family = "Fira Sans"
    ) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot",
      axis.text.x = element_text(size = rel(.8)),
      strip.text = element_text(size = rel(1.1))
    )

  print(p)
}

# Color palette for learner groups
palette_groups = RColorBrewer::brewer.pal(4, "Dark2")
names(palette_groups) = c("Baseline", "Classical", "Trees", "Boosting")


#' Boxplot of scores separated by violation of PH assumption
#'
#' @param xdf A `data.table` as contained in a `BenchmarkAggr`'s `$data` slot.
#' @param eval_measure E.g. `"harrell_c"`.
#' @param tuning_measure E.g. `"harrell_c"`.
#'
#' @return
#'
#' @examples
#' plot_aggr_ph(bma, tuning_measure = "harrell_c")
plot_aggr_ph = function(xdf, eval_measure = "harrell_c", tuning_measure = NULL,
                        learners_exclude = NULL, tasks_exclude = NULL) {
  checkmate::assert_data_table(xdf)
  checkmate::assert_subset(c("tuned", "p"), choices = colnames(xdf))
  checkmate::assert_subset(eval_measure, colnames(xdf))
  checkmate::assert_subset(tuning_measure, unique(xdf[["tuned"]]))

  xdf |>
    dplyr::filter(tuned == tuning_measure) |>
    dplyr::filter(!(eval_measure %in% c("harrell_c", "uno_c")) | !(learner_id %in% c("KM", "NA"))) |>
    ggplot(aes(x = learner_id, y = .data[[eval_measure]], color = p, fill = p)) +
    facet_grid(cols = vars(learner_group), scales = "free_x", space = "free_x") +
    geom_boxplot(alpha = 1/4, key_glyph = "rect") +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
    labs(
      title = msr_tbl[id == eval_measure, label],
      subtitle = paste("Performance separated by PH violation.",
                       "Based on uncorrected p < 0.05 of global Schoenfeld test per task", sep = "\n"),
      caption = glue::glue("Tuned on {msr_tbl[id == tuning_measure, label]}"),
      x = NULL, y = msr_tbl[id == eval_measure, label],
      color = NULL, fill = NULL
    ) +
    theme_minimal(
      base_size = 15#,
      # Would want to use custom fonts but reproducibility... :(
      # base_family = "Fira Sans"
    ) +
    theme(
      legend.position = "top",
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot"
    )
}

#' Scale a learner score from 0 (baseline) to 1 (best)
#' This can produce values <0 but can should never create values greater 1
scale_score = function(score_learner, score_baseline, score_best, lower_is_better = TRUE) {

  denom = (score_best - score_baseline)

  # I know I did this weirdly but I somehow messed this up multiple times such that
  # I am now scared of thinking about and/or touching it again.
  if (lower_is_better) {
    numera = (score_baseline - score_learner)
    denom = -denom
  } else {
    numera = (score_learner - score_baseline)
  }

  res = numera / denom
  checkmate::assert_numeric(res, upper = 1)

  res
}

#' Find best index depending on whether lower is better
which_best = function(x, lower_is_better = TRUE) {
  # Make sure to return a single value in case of ties
  ifelse(lower_is_better, which.min(x), which.max(x)[[1]])
}

#' Take `aggr_scores` and rescale all scores with `scale_score()`,
#' for measures that are a) not calibration measures b) not ERV measures
#'
#' Resulting scores have same column names as before, but are scaled.
#' This is unfortunate parallelism to msr_tbl and existing plotting functions that rely on it
#' but it's hard to avoid this late in the game.
rescale_aggr_scores = function(aggr_scores, msr_tbl) {
  # Inefficient but prevents silly bug
  aggr_scores_scaled = data.table::copy(aggr_scores)

  # Get measures to scale, excluding ERV (already scaled) and calibration measures
  measures_to_scale = msr_tbl[!erv & type != "Calibration", id]
  measures_to_scale = measures_to_scale[measures_to_scale %in% colnames(aggr_scores_scaled)]

  measures_to_drop = msr_tbl[erv | type == "Calibration", id]
  aggr_scores_scaled[, (measures_to_drop) := NULL]

  for (eval_measure in measures_to_scale) {

    # cli::cli_alert_info("scaling for {eval_measure}")
    #if (eval_measure == "harrell_c") browser()

    lower_is_better = msr_tbl[id == eval_measure, minimize]

    scores_km = aggr_scores_scaled[learner_id == "KM", c("task_id", "tuned", ..eval_measure)]
    setnames(scores_km, c(eval_measure), "score_km")

    scores_best = aggr_scores_scaled[
      , .SD[which_best(.SD[[eval_measure]], lower_is_better = lower_is_better), ],
      by = .(task_id, tuned)
    ][, .SD, .SDcols = c("task_id", "tuned", eval_measure)]
    setnames(scores_best, c(eval_measure), "score_best")

    scores_to_scale_by = scores_km[scores_best, on = c("task_id", "tuned")]
    aggr_scores_scaled = aggr_scores_scaled[scores_to_scale_by, on = c("task_id", "tuned")]

    aggr_scores_scaled[ , (eval_measure) := scale_score(
      score_learner = .SD[[eval_measure]],
      score_baseline = score_km,
      score_best = score_best,
      lower_is_better = lower_is_better
    ), .SDcols = eval_measure]

    if (lower_is_better & any(aggr_scores_scaled[[eval_measure]] > 1)) browser()

    aggr_scores_scaled[, score_best := NULL]
    aggr_scores_scaled[, score_km := NULL]
    aggr_scores_scaled[]
  }

  aggr_scores_scaled
}

# Utilities for job management ----------------------------------------------------------------


#' Assemble augmented batchtools job table
#'
#' Includes regular `getJobTable()` info but also task data (n, p, ...) and
#' resource usage estimates.
#'
#' @param reg Registry, defaulting to `getDefaultRegistry()`.
#' @param task_tab_file Path to CSV file as created by `save_tasktab()`.
#' @param resource_est_file See `attic/resource-estimate.qmd`.
#' @param keep_columns Character vector of columsn from `getJobtTable()` to keep.
#'
#' @return A data.table keyed with `job.id`.
#'
#' @examples
#' collect_job_table()
collect_job_table = function(
    reg = batchtools::getDefaultRegistry(),
    task_tab_file = here::here("attic", "tasktab.csv"),
    resource_est_file = here::here("attic", "resource_est_dec.csv"),
    keep_columns = c("job.id", "repl", "tags", "task_id", "learner_id", "log.file", "job.name"),
    optional_columns = c("batch.id", "comment", "memory")
    ) {
  alljobs = unwrap(getJobTable(reg = reg))
  checkmate::assert_data_table(alljobs, min.rows = 1)

  alljobs = alljobs[, c(keep_columns, optional_columns[optional_columns %in% names(alljobs)])
                    , with = FALSE]

  data.table::setnames(alljobs, "tags", "measure")

  tasktab = read.csv(task_tab_file)

  # Get resource estimates
  resource_tab = read.csv(resource_est_file)
  data.table::setDT(resource_tab)
  resource_tab = resource_tab[, c("learner_id", "task_id", "hours", "total_h", "mem_gb")]
  # Reuse XGB results for XGBCox / AFT split
  resource_tab[learner_id == "XGB", learner_id := "XGBcox"]
  xgbaft = data.table::copy(resource_tab[learner_id == "XGBcox",])
  xgbaft[, learner_id := "XGBAFT"]

  resource_tab = rbind(resource_tab, xgbaft)

  # Join everything
  alljobs = ljoin(alljobs, tasktab, by = "task_id")
  alljobs = ljoin(alljobs, resource_tab, by = c("task_id", "learner_id"))
  data.table::setkey(alljobs, job.id)

  alljobs
}

#' Aggregate job status by measure
#'
#' @param alljobs Job table as returned by `collect_job_table()`. notably with column `measure`.
#'   Will call `collect_job_table()` if not provided.
#' @param byvars `character` Vector of variables in `alljobs` to group by, default is `"measure"`
#'
#' @return A `data.table` with main column `measure` and one column for each job state
#'   (`queued`, `running`, `errored`, ...)
#' @example
#' check_job_state()
#' check_job_state(byvars = c("measure", "learner_id"))
#' check_job_state(byvars = "")
check_job_state = function(alljobs = NULL, byvars = "measure") {
  if (is.null(alljobs)) {
    alljobs = collect_job_table()
  }

  job_n = alljobs[, .(total = .N), by = byvars]

  state_tab = data.table::rbindlist(list(
    alljobs[findDone(), .(n = .N, state = "done"), by = byvars],
    alljobs[findRunning(), .(n = .N, state = "running"), by = byvars],
    alljobs[findErrors(), .(n = .N, state = "errored"), by = byvars],
    alljobs[findExpired(), .(n = .N, state = "expired"), by = byvars],
    alljobs[findQueued(), .(n = .N, state = "queued"), by = byvars],
    alljobs[findNotSubmitted(), .(n = .N, state = "not_submitted"), by = byvars]
  ))[!is.na(n), ]

  if (identical(byvars, "")) {
    state_tab = state_tab[job_n, on = byvars]
  } else {
    state_tab[, total := nrow(alljobs)]
  }

  state_tab = state_tab[, perc := round(100 * n / total, 1)]
  state_tab = state_tab[, val := sprintf("%3.1f%% (%i)", perc, n)][]
  state_tab[, n := NULL]
  state_tab[, perc := NULL]
  state_tab[, total := NULL]
  state_tab = data.table::dcast(
    state_tab, ...  ~ state,
    fill = "\u2014", value.var = "val",
    fun.aggregate = identity
  )

  # Sorting cols like this feels less awkward than in base/dt I guess
  dplyr::select(state_tab, dplyr::any_of(byvars), dplyr::any_of(c("not_submitted", "queued", "running", "errored", "expired", "done")))
}


# Debug utilities -----------------------------------------------------------------------------

#' Get the object size of job results
#'
#' In the context of the known issue with the serialization of R6 objects,
#' this `loadResult()`s a job result and uses `pryr::object_size` to get its size.
#'
#' @param ids [`findDone()`] `job.ids` to get result sizes for.
#' @return A data.table with columns `job.id`, `size` (in MiB).
#' @examples
#' res_size = check_result_sizes()
#' jobs_done = alljobs[findDone(), ]
#' jobs_done = jobs_done[res_size, ]
#' jobs_done[, .(avg_size = mean(size)), by = c("learner_id")]
check_result_sizes = function(ids = batchtools::findDone()) {
  if (nrow(ids) > 0) {
    sizes = vapply(unlist(ids), \(x) {
      batchtools::loadResult(x) |>
        pryr::object_size() |>
        as.numeric()
    }, FUN.VALUE = 1)
    ids[, size_bytes := prettyunits::pretty_bytes(bytes = sizes)]
    ids[, size := sizes / 1024^2][]
  } else {
    message("Don't know yet.")
  }
}

#' For quicker aggregation
aggr_result_sizes = function(ids = batchtools::findDone(), by = "learner_id") {
  res_size = check_result_sizes(ids = ids)
  jobs_done = unwrap(getJobTable())[findDone(), ]
  jobs_done = jobs_done[res_size, ]
  jobs_done[, .(avg_size = mean(size)), by = by][]
}


# Misc utils ----------------------------------------------------------------------------------

ensure_directory = function(x) {
  if (!fs::dir_exists(x)) {
    fs::dir_create(x, recurse = TRUE)
  }

  fs::dir_exists(x)
}


#' Bulk-convert datasets from RDS to ARFF.
#'
#' @param data_dir (`character(1)`) Directory to look for datasets, defaults to `here::here("datasets")`.
#' @param overwrite (`logical(1)`) Overwrite existing arff files? Defaults to `TRUE`.
#'
#' @return Invisivbly: `TRUE` if all arff files exists, `FALSE` otherwise.
#' @examples
#' convert_tasks_arff()
convert_tasks_arff = function(data_dir = here::here("datasets"), overwrite = TRUE) {
  if (!requireNamespace("farff", quietly = TRUE)) {
    stop("Please install 'farff' package to use this function.")
  }

  data_rds = fs::dir_ls(data_dir, glob = "*.rds")

  res = vapply(data_rds, \(path) {
    path_arff = fs::path_ext_set(path, "arff")
    path_csv = fs::path_ext_set(path, "csv")

    xdat = readRDS(path)

    cli::cli_alert_info("Converting {.file {fs::path_file(path)}} to arff")
    farff::writeARFF(
      x = xdat,
      path = path_arff,
      overwrite = overwrite,
      relation = fs::path_ext_remove(fs::path_file(path))
    )

    cli::cli_alert_info("Converting {.file {fs::path_file(path)}} to CSV")
    readr::write_csv(
      x = xdat,
      file = path_csv,
      append = !overwrite
    )

    fs::file_exists(path_arff) & fs::file_exists(path_csv)
  }, logical(1))

  invisible(all(res))

}

.canary = "Hello! This variable is here to indicate that the helper functions have been loaded."

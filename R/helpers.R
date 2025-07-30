# cli::cli_alert_info("Loading helpers.R")

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

  file_csv <- fs::path(resampling_dir, task$id, ext = "csv")
  cli::cli_alert_info("Saving resampling to {.file {fs::path_rel(file_csv)}}")

  resampling_tab = as.data.table(resampling)
  readr::write_csv(x = resampling_tab, file = file_csv, append = FALSE)
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
#' @example
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
  resampling_csv = data.table::fread(resampling_csv_path, key = c("set", "iteration"))

  checkmate::assert_data_table(resampling_csv, min.rows = 1L, col.names = "unique", any.missing = FALSE)
  checkmate::assert_names(names(resampling_csv), permutation.of = c("set", "iteration", "row_id"))
  checkmate::assert_set_equal(unique(resampling_csv$set), c("train", "test"))
  checkmate::assert_integerish(resampling_csv$iteration, lower = 1L, any.missing = FALSE)
  checkmate::assert_integerish(resampling_csv$row_id, any.missing = FALSE)

  row_id = NULL
  resampling = ResamplingCustom$new()
  resampling$instance = list(
    train_sets = resampling_csv[list("train"), list(ids = list(row_id)), by = "iteration"]$ids,
    test_sets = resampling_csv[list("test"), list(ids = list(row_id)), by = "iteration"]$ids
  )
  resampling
}

#' Store additional data for tasks
#'
#' Useful to augment job table and later results with e.g. n, p, number of unique event times, ...
#' Repeatedly refreshed in case there are changes with the included tasks in the benchmark.
#' Written to CSV such that possibly unintended changes are immediately obvious via `git status`.
#'
#' @param tasks List of `TaskSurv` objects.
#' @param path Path to store CSV file of results.
#' @example
#'
#' tasks = load_task_data()
#' save_tasktab(tasks)
save_tasktab = function(tasks, path = here::here("tables", "tasktab.csv")) {
  ensure_directory(path)
  task_names = names(tasks)

  # Save overview of tasks with some metadata which comes in handy later
  tasktab = data.table::rbindlist(lapply(seq_along(tasks), \(x) {
    task_data = tasks[[x]]

    if (!(inherits(task_data, "TaskSurv"))) {
      task = as_task_surv(task_data, target = "time", event = "status", id = task_names[x])
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
    "%s__%s__%s__%s__%i.rds",
    callback$state$tuning_measure,
    callback$state$learner_id,
    task_id,
    iter_hash,
    as.integer(Sys.time()) # unix epoch for good measure and sortability
  )
  # Assemble path based on directory and filename, store in state just in case.
  callback$state$path = fs::path(callback$state$path_dir, callback$state$file_name)

  archive = data.table::as.data.table(context$instance$archive)
  archive[, base_id := callback$state$learner_id]
  archive[, task_id := ..task_id]
  archive[, tuning_measure := callback$state$tuning_measure]
  archive[, resampling_hash := ..iter_hash]

  # cli::cli_alert_info("Writing archive to {callback$state$path}")
  saveRDS(archive, callback$state$path)
}

#' Callback to find the learner logs and append them to the tuning archive.
#' Archives get serialized to disk and can later be recovered to find
#' error messages which would otherwise be obscured by using the fallback learners.
#' @return Nothing, but modifies the `context$aggregated_performance` object.
callback_archive_logs_impl = function(callback, context) {
  states = context$benchmark_result$.__enclos_env__$private$.data$learners(states = TRUE)

  logs = data.table::rbindlist(lapply(states$learner, \(x) {
    x$state$log
  }))

  # Only attach log if there's something to attach
  if (nrow(logs) > 0) {
    context$aggregated_performance[, log := list(logs)]
  }
}

#' Assign number of repeats
#'
#' Number of repeats depending on events in dataset.
#' @param num_events Integer number of events
#'
#' @return Integer number of repeats
assign_repeats = function(num_events) {
  data.table::fcase(
    num_events < 500,
    10,
    num_events >= 500 & num_events < 2500,
    5,
    num_events > 2500,
    1
  )
}


# Utilities for analysis ----------------------------------------------------------------------
# fmt: skip
#' List of included learners with short IDs and some extra info
#'
#' @param path Path to store CSV file.
#'
#' @example
#' save_lrntab()
save_lrntab <- function(path = here::here("tables", "learners.csv")) {
  require(mlr3proba)
  requireNamespace("mlr3extralearners", quietly = TRUE)
  ensure_directory(path)

  lrntab <- mlr3misc::rowwise_table(
    ~id,      ~base_id,      ~base_lrn,            ~params, ~encode, ~internal_cv, ~grid,  ~scale, ~surv_pred,
    "KM"      , "kaplan"     , "surv.kaplan"       , 0 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "NEL"     , "nelson"     , "surv.nelson"       , 0 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "AK"      , "akritas"    , "surv.akritas"      , 1 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "CPH"     , "cph"        , "surv.coxph"        , 0 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "GLMN"    , "cv_glmnet"  , "surv.cv_glmnet"    , 1 ,    FALSE , TRUE  ,        FALSE, FALSE,   TRUE,
    "Pen"     , "penalized"  , "surv.penalized"    , 2 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "AFT"     , "parametric" , "surv.parametric"   , 1 ,    FALSE , FALSE ,        TRUE , FALSE,   TRUE,
    "Flex"    , "flexible"   , "surv.flexible"     , 1 ,    FALSE , FALSE ,        TRUE , FALSE,   TRUE,
    "RFSRC"   , "rfsrc"      , "surv.rfsrc"        , 5 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "RAN"     , "ranger"     , "surv.ranger"       , 5 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "CIF"     , "cforest"    , "surv.cforest"      , 5 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "ORSF"    , "aorsf"      , "surv.aorsf"        , 2 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "RRT"     , "rpart"      , "surv.rpart"        , 1 ,    FALSE , FALSE ,        TRUE,  FALSE,   FALSE,
    "MBSTCox" , "mboost_cox" , "surv.mboost"       , 4 ,    FALSE , FALSE ,        FALSE, FALSE,   TRUE,
    "MBSTAFT" , "mboost_aft" , "surv.mboost"       , 4 ,    FALSE , FALSE ,        FALSE, FALSE,   FALSE,
    "CoxB"    , "coxboost"   , "surv.cv_coxboost"  , 0 ,    TRUE  , TRUE  ,        FALSE, FALSE,   TRUE,
    "XGBCox"  , "xgb_cox"    , "surv.xgboost.cox"  , 5 ,    TRUE  , FALSE ,        FALSE, FALSE,   TRUE,
    "XGBAFT"  , "xgb_aft"    , "surv.xgboost.aft"  , 7 ,    TRUE  , FALSE ,        FALSE, FALSE,   FALSE,
    "SSVM"    , "svm"        , "surv.svm"          , 4 ,    TRUE  , FALSE ,        FALSE, TRUE,    FALSE
  )

  lrntab$has_threads = vapply(lrntab$base_lrn, \(x) {
    "threads" %in% unlist(lrn(x)$param_set$tags)
    }, logical(1))

  if (fs::file_exists(path)) {
    cli::cli_alert_warning("Overwriting {.val {fs::path_rel(path)}}")
  }

  readr::write_csv(x = lrntab, file = path, append = FALSE)

  lrntab
}

load_lrntab = function(path = here::here("tables", "learners.csv")) {
  ensure_directory(path)
  cli::cli_alert_info("Loading {.file {path}}")
  data.table::fread(path, na.strings = "")
}

load_tasktab = function(path = here::here("tables", "tasktab.csv")) {
  ensure_directory(path)
  cli::cli_alert_info("Loading {.file {path}}")
  data.table::fread(path)
}

# fmt: skip
#' Quickly recreate list of evaluation measures
#'
#' Define once, use everywhere.
get_measures_eval = function() {
  measures_eval = list(
    msr("surv.cindex",      id = "harrell_c",                      label = "Harrell's C"),
    msr("surv.cindex",      id = "uno_c",      weight_meth = "G2", label = "Uno's C"),

    # Removed RISLL -> added ISLL
    msr("surv.intlogloss",  id = "isll",     p_max = 0.8, ERV = FALSE, proper = FALSE, label = "Integrated Survival Log-Likelihood (ISLL)"),
    msr("surv.intlogloss",  id = "isll_erv", p_max = 0.8, ERV = TRUE,  proper = FALSE, label = "Integrated Survival Log-Likelihood (ISLL) [ERV]"),

    msr("surv.brier",       id = "isbs",     p_max = 0.8, proper = FALSE,  ERV = FALSE, label = "Integrated Survival Brier Score (ISBS)"),
    msr("surv.brier",       id = "isbs_erv", p_max = 0.8, proper = FALSE,  ERV = TRUE,  label = "Integrated Survival Brier Score (ISBS) [ERV]"),

    msr("surv.dcalib",      id = "dcalib", truncate = 1000, label = "D-Calibration"),
    msr("surv.calib_alpha", id = "alpha_calib", method = "ratio", truncate = 1000, label = "Van Houwelingen's Alpha")

  )
  names(measures_eval) = mlr3misc::ids(measures_eval)
  measures_eval
}

# fmt: skip
#' Table of evaluation measures with metadata
#'
measures_tbl = function() {
  msr_tbl = mlr3misc::rowwise_table(
    ~mlr_id,            ~id,           ~type,
    "surv.cindex",      "harrell_c",    "Discrimination",
    "surv.cindex",      "uno_c",        "Discrimination",

    "surv.isll",        "isll",         "Scoring Rule",
    "surv.isll",        "isll_erv",     "Scoring Rule",

    "surv.brier",       "isbs",         "Scoring Rule",
    "surv.brier",       "isbs_erv",     "Scoring Rule",

    "surv.dcalib",      "dcalib",       "Calibration",
    "surv.calib_alpha", "alpha_calib",  "Calibration"
  )

  measures_eval = get_measures_eval()
  msr_tbl = data.table(
    measure = measures_eval,
    id = mlr3misc::ids(measures_eval)
  )[msr_tbl, on = "id"]

  msr_tbl[, label := vapply(msr_tbl$measure, \(x) x$label, "", USE.NAMES = FALSE)]
  # ERV is either a logical param or not present --> implies ERV = FALSE
  msr_tbl[, erv := vapply(measure, \(x) isTRUE(x$param_set$values$ERV), logical(1), USE.NAMES = FALSE)]

  # Minimize is taken from measure-encoded value, but overridden if ERV = TRUE as ERV can't be minimized
  msr_tbl[, minimize := vapply(measure, \(x) x$minimize, logical(1), USE.NAMES = FALSE)]
  msr_tbl[, minimize := fifelse(erv, FALSE, minimize)]

  msr_tbl[]
}

#' Collect tuning archives saved separately to disk via callback
#' @param conf `config::get()` for result paths
#' @param keep_logs [`TRUE`] Whether to keep the logs, which drastically increases the size
#'   of the resulting object in memory and on disk
#' @param ignore_cache [`FALSE`] Whether to ignore a cached result and reassemble from scratch
reassemble_archives = function(
  conf = config::get(),
  keep_logs = TRUE,
  ignore_cache = FALSE
) {
  if (keep_logs) {
    archive_path = fs::path(conf$result_path, "archives-with-logs.rds")
  } else {
    archive_path = fs::path(conf$result_path, "archives.rds")
  }

  ensure_directory(archive_path)

  if (fs::file_exists(archive_path)) {
    if (!ignore_cache) {
      cli::cli_alert_info("Archives already aggregated, returning cache from {.file {fs::path_rel(archive_path)}}")
      return(readRDS(archive_path))
    } else {
      cli::cli_alert_info("Ignoring that {.file {fs::path_rel(archive_path)}} is already present, overwriting...")
    }
  }

  archive_dir = fs::path(conf$reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)

  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {.file {fs::path_rel(archive_dir)}}!")
  }

  learners = load_lrntab()
  learners = learners[, c("id", "base_id")]
  names(learners) = c("learner_id", "base_id")

  cli::cli_h3("Reading tuning archives")
  .pb = new.env()
  cli::cli_progress_bar("Reading archives from disk", .envir = .pb)
  archives = data.table::rbindlist(lapply(tuning_files, \(file) {
    # cli::cli_progress_step(msg = "{.file {file}}")
    cli::cli_progress_update(.envir = .pb)
    archive <- tryCatch(
      {
        readRDS(file)
      },
      error = function(e) {
        cli::cli_warn("Error reading {.file {file}}: {.val {e$message}}")
        NULL
      }
    )
    if (is.null(archive)) {
      return()
    }

    # Since we keep track of the tuning measure as a separate variable,
    # it's easier to rename the score variale to later rbind the tuning archives (I think)
    data.table::setnames(
      archive,
      old = c("harrell_c", "isbs"),
      new = c("score", "score"),
      skip_absent = TRUE
    )

    if (!keep_logs & "log" %in% names(archive)) {
      # Temp fix because objects became to large
      archive[, log := NULL]
    }

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")
    components = components[[1]]
    names(components) = c("tuning_measure", "base_id", "task_id", "iter_hash", "time_epoch")

    ret = data.table::data.table(
      t(components),
      archive = list(archive),
      file = fs::path_rel(file),
      warnings_sum = sum(archive$warnings),
      errors_sum = sum(archive$errors)
    )
  }))
  cli::cli_progress_done(.envir = .pb)

  archives = archives[learners, on = "base_id"]
  archives[, base_id := NULL]
  archives = archives[!is.na(tuning_measure), ]
  data.table::setcolorder(
    archives,
    c(
      "learner_id",
      "task_id",
      "tuning_measure",
      "iter_hash",
      "time_epoch",
      "warnings_sum",
      "errors_sum",
      "archive",
      "file"
    )
  )
  saveRDS(archives, archive_path)

  archives[]
}

#' Read tuning archives stored in registry and convert to CSV in results dir
#' @param conf `config::get()` for result paths.
#' @param verbose `FALSE` Diligently report file writes.
#' @return Nothing, only writes files.
convert_archives_csv = function(conf = config::get(), verbose = FALSE) {
  archive_csv_dir = fs::path(conf$result_path, "tuning_archives")
  ensure_directory(archive_csv_dir)

  archive_dir = fs::path(conf$reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)

  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {.file {fs::path_rel(archive_dir)}}!")
  }

  # Having to fix XGBoost split not corresponding to learner IDs at time of benchmark
  learners = load_lrntab()
  learners = learners[, c("id", "base_id")]

  pb = cli::cli_progress_bar("Reading tuning archives", total = length(tuning_files))
  purrr::walk(tuning_files, \(file) {
    cli::cli_progress_update(id = pb)
    archive = readRDS(file)

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")

    components = components[[1]]
    names(components) = c("tuning_measure", "base_id", "task_id", "iter_hash", "time_epoch")
    components = components[c("tuning_measure", "base_id", "task_id", "iter_hash")]

    to_csv = cbind(
      data.table::data.table(t(components[c("tuning_measure", "base_id", "task_id", "iter_hash")])),
      archive
    )

    out_path = fs::path(archive_csv_dir, paste(components, collapse = "__"), ext = "csv")
    if (!fs::file_exists(out_path)) {
      if (verbose) {
        cli::cli_alert_info(
          "Writing archive for {.val {components[['learner_id_long']]}} tuned with \\
                          {.val {components[['tuning_measure']]}} on {.val {components[['task_id']]}} \\
                          (iter {.val {components[['iter_hash']]}}) to CSV"
        )
      }

      readr::write_csv(x = to_csv, file = out_path, append = FALSE)
    }
  })
  cli::cli_progress_done(id = pb)
}


#' Removing duplicate tuning archives
#'
#' When jobs are resubmitted, the previous tuning archive is not removed automatically,
#' so the associated time_epoch is used to identify the older archive and move it to
#' a backup location.
#' @param conf `config::get()`
#' @param tmp_path `here::here("tmp", "archive-backup")`.
#'
#' @example
#' clean_duplicate_archives(config::get())
clean_duplicate_archives = function(
  conf,
  tmp_path = here::here("tmp", "archive-backup")
) {
  ensure_directory(tmp_path)

  archive_dir = fs::path(conf$reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)

  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {.file {fs::path_rel(archive_dir)}}!")
  }

  archive_meta = data.table(file = tuning_files)
  archive_meta[,
    c("tuning_measure", "learner_base_id", "task_id", "iter_hash", "time_epoch") := {
      fs::path_file(file) |>
        fs::path_ext_remove() |>
        stringr::str_split_fixed("__", n = 5) |>
        as.data.table()
    }
  ]

  # Sort such that earlier time_epoch appears later, e.g. first entry is always the most recent one per group
  # archive_meta = archive_meta[order(tuning_measure, learner_base_id, task_id, iter_hash, -time_epoch)]
  setorder(archive_meta, tuning_measure, learner_base_id, task_id, iter_hash, -time_epoch)

  # archive_meta[tuning_measure == "harrell_c" & learner_base_id == "xgb_cox" & task_id == "whas"]
  # archive_meta[
  #   tuning_measure == "isbs" &
  #     learner_base_id == "xgb_cox" &
  #     task_id == "colrec" &
  #     iter_hash == "05b206f8c40e076e41a63aec61e6e974"
  # ]

  archive_meta[, counter := rowid(tuning_measure, learner_base_id, task_id, iter_hash)]
  n_duplicates = nrow(archive_meta[counter > 1])

  if (n_duplicates == 0) {
    cli::cli_alert_success(
      "Found no duplicate archives across {.val tuning_measure}{.val learner_base_id}{.val task_id}{.val iter_hash}"
    )
    return(TRUE)
  }
  cli::cli_inform(c(
    i = "Found {.val {n_duplicates}} duplicate archives total across {.val tuning_measure}, {.val learner_base_id}, {.val task_id}, {.val iter_hash}",
    "!" = "Keeping most recent archives only, moving duplicates to {.file {tmp_path}}"
  ))

  archive_meta[tuning_measure == "isbs" & learner_base_id == "xgb_cox" & task_id == "colrec"]

  files_to_move = archive_meta[counter > 1, file]
  fs::file_move(files_to_move, tmp_path)

  cli::cli_alert_success("Done!")
}

#' Check if scores are valid / invalid
#'
#' Validity in this case only meaning it's neither Inf, NA, nor NaN
#' @param x `numeric()`
is_valid = function(x) is.finite(x) & !is.na(x) & !is.nan(x)
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
remove_results = function(x, learner_id_exclude = NULL, task_id_exclude = NULL) {
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

  if (inherits(x, "BenchmarkAggr")) {
    return(mlr3benchmark::as_benchmark_aggr(xdat))
  }
  xdat[]
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
        c("KM", "NEL", "AK") ~ "Baseline",
        c("CPH", "GLMN", "Pen", "AFT", "Flex", "SSVM") ~ "Classical",
        c("RRT", "RFSRC", "RAN", "CIF", "ORSF") ~ "Trees",
        c("MBSTCox", "MBSTAFT", "XGBCox", "XGBAFT", "CoxB") ~ "Boosting",
        .ptype = factor(levels = c("Baseline", "Classical", "Trees", "Boosting"))
      )
    )
}

tablify = function(x, caption = NULL, ...) {
  x |>
    kableExtra::kbl(caption = caption, booktabs = TRUE, ...) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
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

  if (any(is.infinite(res)) | anyNA(res)) {
    cli::cli_inform(
      c(
        "Generated missing or infinite values by rescaling:",
        "!" = "{.val {sum(is.infinite(res))}} Infs",
        "!" = "{.val {sum(is.nan(res))}} NaNs",
        "!" = "{.val {sum(is.na(res))}} NAs",
        "!" = "{.val {sum(res < 0, na.rm = TRUE)}} negative values",
        "!" = "{.val {sum(res > 1, na.rm = TRUE)}} values > 1",
        "Of {.val {length(res)}} values total."
      )
    )
  }

  # Manual fixing for scores with NaN, Inf
  res[denom == 0] <- 0
  res[!is.finite(res)] <- NA_real_
  res[res < 0] <- 0
  res[res > 1] <- 1
  res
}

#' Find best index depending on whether lower is better
which_best = function(x, lower_is_better = TRUE) {
  # Make sure to return a single value in case of ties
  ifelse(lower_is_better, which.min(x), which.max(x))[[1]]
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

    scores_km = aggr_scores_scaled[learner_id == "KM", c("task_id", "tune_measure", ..eval_measure)]
    setnames(scores_km, old = c(eval_measure), new = "score_km")
    scores_km[, tune_measure := NULL]

    scores_best = aggr_scores_scaled[,
      .SD[which_best(.SD[[eval_measure]], lower_is_better = lower_is_better), ],
      by = c("task_id", "tune_measure")
    ][, .SD, .SDcols = c("task_id", "tune_measure", eval_measure)]
    setnames(scores_best, old = c(eval_measure), new = "score_best")

    scores_to_scale_by = scores_km[scores_best, on = c("task_id")]
    aggr_scores_scaled = aggr_scores_scaled[scores_to_scale_by, on = c("task_id", "tune_measure")]

    aggr_scores_scaled[,
      (eval_measure) := scale_score(
        score_learner = .SD[[eval_measure]],
        score_baseline = score_km,
        score_best = score_best,
        lower_is_better = lower_is_better
      ),
      .SDcols = eval_measure
    ]

    # if (lower_is_better & any(aggr_scores_scaled[[eval_measure]] > 1)) browser()

    aggr_scores_scaled[, score_best := NULL]
    aggr_scores_scaled[, score_km := NULL]
    aggr_scores_scaled[]
  }

  aggr_scores_scaled
}


#' Convert scores to aggregated result
#' From one score per learner/task/outer resampling/tuning measure
#' to
#' learner/task/tuning measure
#' The result also serves the basis for the bma object used with mlr3benchmark
#' @param scores A data.table as produced by `bmr$score()`, including a `tune_measure` column (see `process-results.R`)
#' @param msr_tbl Table of measures used to identify relevant columns, see `measures_tbl()`
#'
scores_to_aggr = function(scores, msr_tbl) {
  # Sum up errors, warnings, iters to get the correct totals
  aggr_errwrns_tmp = scores[,
    .(
      iters = max(iteration),
      warnings_cnt = sum(warnings_cnt),
      errors_cnt = sum(errors_cnt)
    ),
    by = .(learner_id, task_id, tune_measure)
  ]

  # Columns with scores vary based on tuning measure, so we detect them manually
  measure_columns = mlr3misc::keep(colnames(scores), colnames(scores) %in% msr_tbl$id)

  # Average the scores... again. At this point we might as well not $aggregate() at all and just do this directly.
  aggr_scores_tmp = scores[,
    lapply(.SD, mean, na.rm = TRUE),
    by = .(learner_id, task_id, tune_measure),
    .SDcols = measure_columns
  ]

  aggr_scores_tmp[aggr_errwrns_tmp, on = .(learner_id, task_id, tune_measure)][]
}

# Utilities for job management ----------------------------------------------------------------

#' Assemble augmented batchtools job table
#'
#' Includes regular `getJobTable()` info but also task data (n, p, ...) and
#' resource usage estimates.
#'
#' @param reg Registry, defaulting to `getDefaultRegistry()`.
#' @param resource_est_file See `resource-estimate.qmd`.
#' @param keep_columns Character vector of columns from `getJobtTable()` to keep.
#'
#' @return A data.table keyed with `job.id`.
#'
#' @example
#' collect_job_table()
collect_job_table = function(
  reg = batchtools::getDefaultRegistry(),
  resource_est_file = here::here("tables", "resource_estimates.csv"),
  keep_columns = c("job.id", "repl", "tags", "task_id", "learner_id", "log.file", "job.name"),
  optional_columns = c("batch.id", "comment", "memory")
) {
  tab = unwrap(getJobTable(reg = reg))
  checkmate::assert_data_table(tab, min.rows = 1)

  tab = tab[, c(keep_columns, optional_columns[optional_columns %in% names(tab)]), with = FALSE]

  data.table::setnames(tab, "tags", "measure")

  tasktab = load_tasktab()

  # Get resource estimates
  if (fs::file_exists(resource_est_file)) {
    resource_tab = data.table::fread(resource_est_file)
    resource_tab = resource_tab[, c("learner_id", "task_id", "est_total_hours", "est_total_hours_raw", "est_mem_mb")]
    tab = ljoin(tab, resource_tab, by = c("task_id", "learner_id"))
  }

  # Join everything
  tab = ljoin(tab, tasktab, by = "task_id")
  data.table::setkey(tab, job.id)

  tab
}

#' Aggregate job status by measure
#'
#' @param tab Job table as returned by `collect_job_table()`. notably with column `measure`.
#'   Will call `collect_job_table()` if not provided.
#' @param by `character(1)` Variables in `tab` to group by, default is `"measure"`
#'
#' @return A `data.table` with main column `measure` and one column for each job state
#'   (`queued`, `running`, `errored`, ...)
#' @example
#' check_job_state()
#' check_job_state(by = "learner_id")
#' check_job_state(by = NULL)
check_job_state = function(tab = NULL, by = "measure") {
  if (is.null(tab)) {
    tab = collect_job_table()
  }
  if (length(by == 1)) {
    checkmate::assert_subset(by, choices = names(tab))
  }

  state_tab = data.table::rbindlist(lapply(unique(tab[[by]]), \(cat) {
    tbl = batchtools:::getStatusTable(ids = tab[tab[[by]] == cat, .(job.id)])
    tbl[, group := cat]
  }))
  setnames(state_tab, "group", by)

  # Sorting cols like this feels less awkward than in base/dt I guess
  dplyr::select(
    state_tab,
    dplyr::any_of(by),
    dplyr::any_of(c("defined", "submitted", "queued", "running", "errored", "expired", "done"))
  ) |>
    dplyr::mutate(dplyr::across(submitted:done, \(x) {
      data.table::fcase(
        x == 0,
        "\u2014",
        x / defined == 1,
        "\u2705",
        x < defined,
        sprintf("%3.1f%% (%i)", 100 * x / defined, x)
      )
    }))
}

# Debug utilities -----------------------------------------------------------------------------

#' Get the object size of job results
#'
#' In the context of the known issue with the serialization of R6 objects,
#' this `loadResult()`s a job result and uses `pryr::object_size` to get its size.
#'
#' @param ids [`findDone()`] `job.ids` to get result sizes for.
#' @return A data.table with columns `job.id`, `size` (in MiB).
#' @example
#' res_size = check_result_sizes()
#' jobs_done = tab[findDone(), ]
#' jobs_done = jobs_done[res_size, ]
#' jobs_done[, .(avg_size = mean(size)), by = c("learner_id")]
check_result_sizes = function(ids = batchtools::findDone()) {
  if (nrow(ids) > 0) {
    sizes = vapply(
      unlist(ids),
      \(x) {
        batchtools::loadResult(x) |>
          pryr::object_size() |>
          as.numeric()
      },
      FUN.VALUE = 1
    )
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

#' Ensure a directory exists
#'
#' Creates the directory `x`, or if `x` is a file, creates
#' the enclosing directory to ensure file `x` can be created.
#' @param x `character()` A filsystem path to a file or directory.
#' @return `TRUE` if the (enclosing) directory exists, `FALSE` otherwise
ensure_directory = function(x) {
  # If x is not a directory already and has no file extension
  if (!fs::is_dir(x) & fs::path_ext(x) != "") {
    x = fs::path_dir(x)
  }

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

  res = vapply(
    data_rds,
    \(path) {
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
    },
    logical(1)
  )

  invisible(all(res))
}

.canary = "Hello! This variable is here to indicate that the helper functions have been loaded."

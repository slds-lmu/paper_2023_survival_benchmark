# Helpers run pre-benchmark -------------------------------------------------------------------

#' Store instantiated resamplings as mlr3 resampling objects
#' and as portable CSV files to `here::here("resamplings"`)
#'
#' @param resampling Object of class `Resampling`, has to be instantiated.
#' @param task_name String to use as file name.
save_resampling = function(resampling, task_name) {
  if (!dir.exists(here::here("resamplings"))) dir.create(here::here("resamplings"))
  stopifnot(resampling$is_instantiated)

  file_csv <- here::here("resamplings", paste0(task_name, ".csv"))
  write.csv(resampling$instance, file_csv, row.names = FALSE)

  # file_rds <- here::here("resamplings", paste0(task_name, ".rds"))
  # saveRDS(resampling, file_rds)
}

#' Store additional data fro tasks
#'
#' Useful to augment job table and later results with e.g. n, p, number of unique event times, ...
#' Repeatedly refreshed in case there are changes with the included tasks in the benchmark.
#' Written to CSV such that possibly unintended changes are immediately obvious via `git status`.
#'
#' @param tasks List of `TaskSurv` objects.
#' @param path Path to store CSV file of results.
save_tasktab = function(tasks, path = here::here("attic", "tasktab.csv")) {
  # Save overview of tasks with some metadata which comes in handy later
  tasktab = data.table::rbindlist(lapply(tasks, \(x) {
    data.table::data.table(
      task_id = x$id,
      n = x$nrow,
      p = length(x$feature_names),
      dim = x$nrow * length(x$feature_names),
      n_uniq_t = length(unique(x$data(cols = "time")[[1]]))
    )
  }))
  tasktab[, dimrank := data.table::frank(dim)]
  tasktab[, uniq_t_rank := data.table::frank(n_uniq_t)]

  write.csv(tasktab, path, row.names = FALSE)
  tasktab
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
  if (!fs::dir_exists(callback$state$path_dir)) {
    fs::dir_create(callback$state$path_dir)
  }

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
    AF = list(learner = "surv.akritas", params = 1),
    CPH = list(learner = "surv.coxph", params = 0),
    GLM = list(learner = "surv.cv_glmnet", .encode = TRUE, params = 1, internal_cv = TRUE),
    Pen = list(learner = "surv.penalized", params = 2),
    Par = list(learner = "surv.parametric", params = 1, grid = TRUE),
    Flex = list(learner = "surv.flexible", params = 1, grid = TRUE),
    RFSRC = list(learner = "surv.rfsrc", params = 5),
    RAN = list(learner = "surv.ranger", params = 5),
    CIF = list(learner = "surv.cforest", params = 5),
    ORSF = list(learner = "surv.aorsf", params = 2),
    RRT = list(learner = "surv.rpart", params = 1, grid = TRUE),
    MBO = list(learner = "surv.mboost", params = 4),
    CoxB = list(learner = "surv.cv_coxboost", .encode = TRUE, params = 0, internal_cv = TRUE),
    XGBCox = list(learner = "surv.xgboost", .encode = TRUE, params = 6, .form = "ph"),
    XGBAFT = list(learner = "surv.xgboost", .encode = TRUE, params = 6, .form = "aft"),
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

#' Quickly recreate list of evaluation measures
#'
#' Define once, use everywhere.
get_measures_eval = function() {
  measures_eval = list(
    msr("surv.cindex",      id = "harrell_c",                      label = "Harrell's C"),
    msr("surv.cindex",      id = "uno_c",      weight_meth = "G2", label = "Uno's C"),

    msr("surv.rcll",        id = "rcll",     ERV = FALSE,  label = "Right-Censored Log Loss"),
    msr("surv.rcll",        id = "rcll_erv", ERV = TRUE,   label = "Right-Censored Log Loss (ERV)"),

    msr("surv.logloss",     id = "logloss",     ERV = FALSE, label = "Log Loss"),
    msr("surv.logloss",     id = "logloss_erv", ERV = TRUE,  label = "Log Loss (ERV)"),

    msr("surv.intlogloss",  id = "intlogloss",     ERV = FALSE, proper = TRUE, label = "Integrated Log Loss (Proper)"),
    msr("surv.intlogloss",  id = "intlogloss_erv", ERV = TRUE,  proper = TRUE, label = "Integrated Log Loss (Proper, ERV)"),

    msr("surv.calib_alpha", id = "caliba", label = "Van Houwelingen's Alpha"),

    msr("surv.dcalib",      id = "dcalib", truncate = 10, label = "D-Calibration (truncated)"),

    msr("surv.graf",        id = "graf_proper",     proper = TRUE,   ERV = FALSE, label = "Graf Score (Proper)"),
    msr("surv.graf",        id = "graf_proper_erv", proper = TRUE,   ERV = TRUE, label = "Graf Score (Proper, ERV)"),

    msr("surv.graf",        id = "graf_improper",     proper = FALSE,  ERV = FALSE, label = "Graf Score (Improper)"),
    msr("surv.graf",        id = "graf_improper_erv", proper = FALSE,  ERV = TRUE, label = "Graf Score (Improper, ERV)")
  )
  names(measures_eval) = mlr3misc::ids(measures_eval)
  measures_eval
}

#' Table of evaluation measures with metadata
#'
measures_tbl = function() {
  msr_tbl = mlr3misc::rowwise_table(
    ~mlr_id,            ~id,
    "surv.cindex",      "harrell_c",
    "surv.cindex",      "uno_c",

    "surv.rcll",        "rcll",
    "surv.rcll",        "rcll_erv",

    "surv.graf",        "graf_proper",
    "surv.graf",        "graf_proper_erv",

    "surv.graf",        "graf_improper",
    "surv.graf",        "graf_improper_erv",

    "surv.dcalib",      "dcalib",

    "surv.intlogloss",  "intlogloss",
    "surv.intlogloss",  "intlogloss_erv",

    "surv.logloss",     "logloss",
    "surv.logloss",     "logloss_erv",

    "surv.calib_alpha", "caliba"
  )

  measures_eval = get_measures_eval()
  msr_tbl = data.table(
    measure = measures_eval,
    id = mlr3misc::ids(measures_eval)
  )[msr_tbl, on = "id"]

  msr_tbl[, label := vapply(msr_tbl$measure, \(x) x$label, "", USE.NAMES = FALSE)]
  msr_tbl[, erv := grepl("_erv$", id)]
  # minimize argument should be FALSE if measure is ERV or C-index (suffixes _erv or _c)
  msr_tbl[, minimize := !(erv | grepl("_c$", id))]

  msr_tbl[]
}



#' Collect and save benchmark results
#'
# @param reg Registry, defaulting to `getDefaultRegistry()`.
#' @param reg_name Name of registry, e.g. `"registry"`.
#'   Used to load the registry with `writeable = TRUE` to collect results.
#' @param tuning_measure E.g. "harrell_c"
#' @param measures_eval List of mlr3 measures used for evaluation
#' @param result_path `here::here("results")`, where to store results.
#'   A subfolder based on registry folder name will be created.
#'
#' @return Nothing
#'
#' @examples
collect_results = function(
    reg_name,
    tuning_measure = "harrell_c",
    measures_eval = get_measures_eval(),
    result_path = here::here("results"),
    include_aggr = FALSE,
    id_filter = NULL
) {

  reg_dir = here::here(reg_name)
  reg = suppressWarnings(batchtools::loadRegistry(reg_dir, writeable = TRUE))

  cli::cli_alert_info("Using registry {.file '{reg_name}'}")
  result_path = fs::path(result_path, reg_name)
  cli::cli_alert_info("Storing results for '{tuning_measure}' in {fs::path_rel(result_path)}")
  if (!fs::dir_exists(result_path)) fs::dir_create(result_path)

  selected_ids = batchtools::findTagged(tuning_measure, reg = reg)
  done_ids = batchtools::findDone(selected_ids)
  done_perc = round(100 * nrow(done_ids)/nrow(selected_ids), 3)
  cli::cli_alert_info("Selected {nrow(selected_ids)} ids of which {nrow(done_ids)} are done ({done_perc}%)")

  if (!is.null(id_filter)) {
    done_ids = ijoin(done_ids, id_filter)
    cli::cli_alert_info("Filtering down to {nrow(done_ids)} ids")
  }

  path_bmr = glue::glue("{result_path}/bmr_{tuning_measure}.rds")
  path_bmr_tab = glue::glue("{result_path}/bmrtab_{tuning_measure}.rds")
  path_bma = glue::glue("{result_path}/bma_{tuning_measure}.rds")
  path_aggr = glue::glue("{result_path}/aggr_{tuning_measure}.rds")

  if (!file.exists(path_bmr)) {
    tictoc::tic(msg = glue::glue("Reducing results: {tuning_measure}"))
    bmr = reduceResultsBatchmark(
      ids = done_ids,
      store_backends = TRUE, reg = reg
    )
    tictoc::toc()

    gc()

    bmr_tab = bmr$aggregate(measures = list(), conditions = TRUE)
    bmr_tab = data.table::as.data.table(bmr_tab)
    bmr_tab[, resample_result := NULL]
    saveRDS(bmr_tab, path_bmr_tab)

    # benchmark result
    tictoc::tic(msg = glue::glue("Saving bmr: {tuning_measure}"))
    saveRDS(bmr, file = path_bmr)
    tictoc::toc()
  } else if (!fs::file_exists(path_bma) | !fs::file_exists(path_aggr)) {
    tictoc::tic(msg = glue::glue("Reading bmr from disk: {tuning_measure}"))
    bmr = readRDS(path_bmr)
    tictoc::toc()
  } else {
    cli::cli_alert_success("bmr, bma and aggr already exist!")
  }

  gc()

  # bma via mlr3benchmark
  if (!fs::file_exists(path_bma)) {
    tictoc::tic(msg = glue::glue("as_benchmark_aggr'ing results: {tuning_measure}"))
    bma = mlr3benchmark::as_benchmark_aggr(bmr, measures = measures_eval)
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Saving bma: {tuning_measure}"))
    saveRDS(bma, path_bma)
    tictoc::toc()
  } else {
    cli::cli_alert_success("bma already saved!")
  }

  gc()

  # Aggr is probably optional and also takes a while to create
  if (include_aggr) {
    if (!fs::file_exists(path_aggr)) {
      # benchmark$aggregate
      tictoc::tic(msg = glue::glue("$aggregate'ing bmr: {tuning_measure}"))
      aggr = bmr$aggregate(measures = measures_eval, conditions = TRUE)
      tictoc::toc()

      tictoc::tic(msg = glue::glue("Saving aggr: {tuning_measure}"))
      saveRDS(aggr, path_aggr)
      tictoc::toc()
    } else {
      cli::cli_alert_success("aggr already saved!")
    }
  }


}

#' Collect tuning archives saved separately to disk via callback
#' @param reg_name
#' @param result_path `here::here("results")`
reassemble_archives = function(
    reg_name,
    result_path = here::here("results"),
    keep_logs = TRUE
  ) {

  reg_dir = here::here(reg_name)

  if (keep_logs) {
    archive_path = fs::path(result_path, reg_name, "archives-with-logs.rds")
  } else {
    archive_path = fs::path(result_path, reg_name, "archives-no-logs.rds")
  }

  if (fs::file_exists(archive_path)) {
    cli::cli_alert_inform("Archives allready aggregated, returning cache from {.file {fs::path_rel(archive_path)}}")
    return(readRDS(archive_path))
  }

  archive_dir = here::here(reg_dir, "tuning_archives")
  tuning_files = fs::dir_ls(archive_dir)
  if (length(tuning_files) == 0) {
    cli::cli_abort("No tuning archives found in {fs::path_rel(archive_dir)}!")
  }

  learners = read.csv(here::here("attic", "learners.csv"))
  learners = learners[, c("learner_id", "learner_id_long")]

  learners$learner_id_long = dplyr::case_when(
    learners$learner_id == "XGBCox" ~ "surv.xgboostcox",
    learners$learner_id == "XGBAFT" ~ "surv.xgboostaft",
    .default = learners$learner_id_long
  )

  archives = data.table::rbindlist(lapply(tuning_files, \(file) {
    archive = readRDS(file)

    if (!keep_logs) {
      # Temp fix because objects became to large
      # cli::cli_alert_info("Removing logs from archives!")
      archive[, log := NULL]
    }

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")
    components = components[[1]]
    names(components) = c("tune_measure", "learner_id_long", "task_id", "time_epoch", "iter_hash")

    data.table::data.table(t(components),
               archive = list(archive),
               file = file,
               warnings_sum = sum(archive$warnings),
               errors_sum = sum(archive$errors))
  }))

  archives = archives[learners, on = "learner_id_long"]
  archives = archives[!is.na(tune_measure), ]
  archives[, learner_id_long := NULL]

  saveRDS(archives, archive_path)


  archives[]
}

#' Removing duplicate tuning archives
#'
#' When jobs are resubmitted, the previous tuning archive is not removed automatically,
#' so the associated timestamp is used to identify the older archive and move it to
#' a backup location.
#' @param reg_dir Path to the registry (contaisn the archives).
#' @param result_path `here::here("results")`.
#' @param tmp_path `here::here("tmp", "archive-backup")`.
#'
#' @examples
#' clean_duplicate_archives("registry")
clean_duplicate_archives = function(reg_dir, result_path = here::here("results"),
                                    tmp_path = here::here("tmp", "archive-backup")) {

  if (!fs::dir_exists(tmp_path)) {
    fs::dir_create(tmp_path)
  }

  archives = reassemble_archives(reg_dir, result_path = result_path, keep_logs = FALSE)

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

#' Perform global Friedman tests and collect results
#'
#' A thin wrapper around `BenchmarkAggr`'s `$friedman_test()` with some cleanup.
#' @param bma A `BenchmarkAggr` object.
#' @param measures_eval_ids A `character` vector of measure ids present in `bma`.
#' @param digits `[3]` Passed to `format.pval`.
#' @param conf.level `[0.95]` Passed to `format.val(..., eps = 1 - conf.level)`.
aggr_friedman_global = function(bma, measures_eval_ids, digits = 3, conf.level = 0.95) {
  res = data.table::rbindlist(lapply(measures_eval_ids, \(x) {
    ret = broom::tidy(bma$friedman_test(meas = x))
    ret$measure = x
    ret[, c("measure", "statistic", "p.value", "parameter")]
  }))

  res[, p.value.fmt := format.pval(p.value, digits = 3, eps = 1 - conf.level)][]
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

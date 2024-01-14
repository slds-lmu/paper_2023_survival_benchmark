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
    data.table::data.table(
      learner_log = list(x$state$log),
      fallback_log = list(x$state$fallback_state$log)
    )
  }))

  context$aggregated_performance[, log := list(logs)]
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
    msr("surv.cindex", id = "harrell_c"),
    msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
    msr("surv.rcll", id = "rcll"),

    msr("surv.graf", id = "graf_proper", proper = TRUE),
    msr("surv.graf", id = "graf_improper", proper = FALSE),

    msr("surv.dcalib", id = "dcalib", truncate = Inf),

    msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
    msr("surv.logloss", id = "logloss"),
    msr("surv.calib_alpha", id = "caliba")
  )
  names(measures_eval) = mlr3misc::ids(measures_eval)
  measures_eval
}

#' Collect and save benchmark results
#'
#' @param reg Registry, defaulting to `getDefaultRegistry()`.
#' @param tuning_measure E.g. "harrell_c"
#' @param measures_eval List of mlr3 measures used for evaluation
#' @param result_path `here::here("results")`, where to store results.
#'   A subfolder based on registry folder name will be created.
#'
#' @return Nothing
#'
#' @examples
collect_results = function(
    reg = batchtools::getDefaultRegistry(),
    tuning_measure = "harrell_c",
    measures_eval = get_measures_eval(),
    result_path = here::here("results")
) {

  reg_name = fs::path_file(reg$file.dir)
  cli::cli_alert_info("Using registry '{reg_name}'")
  result_path = fs::path(result_path, reg_name)
  cli::cli_alert_info("Storing results for '{tuning_measure}' in {fs::path_rel(result_path)}")
  if (!fs::dir_exists(result_path)) fs::dir_create(result_path)

  selected_ids = findTagged(tuning_measure, reg = reg)
  done_ids = findDone(selected_ids)
  done_perc = round(100 * nrow(done_ids)/nrow(selected_ids), 3)
  cli::cli_alert_info("Selected {nrow(selected_ids)} ids of which {nrow(done_ids)} are done ({done_perc}%)")

  tictoc::tic(msg = glue::glue("Reducing results: {tuning_measure}"))
  bmr = reduceResultsBatchmark(
    ids = done_ids,
    store_backends = TRUE, reg = reg
  )
  tictoc::toc()

  # benchmark result
  tictoc::tic(msg = glue::glue("Saving bmr: {tuning_measure}"))
  saveRDS(bmr, file = glue::glue("{result_path}/bmr_{tuning_measure}.rds"))
  tictoc::toc()

  # bma via mlr3benchmark
  tictoc::tic(msg = glue::glue("as_benchmark_aggr'ing results: {tuning_measure}"))
  bma = mlr3benchmark::as_benchmark_aggr(bmr, measures = measures_eval)
  tictoc::toc()

  tictoc::tic(msg = glue::glue("Saving bma: {tuning_measure}"))
  saveRDS(bma, glue::glue("{result_path}/bma_{tuning_measure}.rds"))
  tictoc::toc()

  # benchmark$aggregate
  tictoc::tic(msg = glue::glue("$aggregate'ing bmr: {tuning_measure}"))
  aggr = bmr$aggregate(measures = measures_eval, conditions = TRUE)
  tictoc::toc()

  tictoc::tic(msg = glue::glue("Saving aggr: {tuning_measure}"))
  saveRDS(aggr, glue::glue("{result_path}/aggr_{tuning_measure}.rds"))
  tictoc::toc()

}

#' Collect tuning archives saved separately to disk via callback
#' @param reg_dir
#' @param result_path `here::here("results")`
reassemble_archives = function(
    reg_dir,
    result_path = here::here("results")
  ) {

  tuning_files = fs::dir_ls(here::here(reg_dir, "tuning_archives"))

  learners = read.csv(here::here("attic", "learners.csv"))
  learners = learners[, c("learner_id", "learner_id_long")]

  learners$learner_id_long = dplyr::case_when(
    learners$learner_id == "XGBCox" ~ "surv.xgboostcox",
    learners$learner_id == "XGBAFT" ~ "surv.xgboostaft",
    .default = learners$learner_id_long
  )

  archives = data.table::rbindlist(lapply(tuning_files, \(file) {
    archive = readRDS(file)

    # Temp fix because objects became to large
    cli::cli_alert_info("Removing logs from archives!")
    archive[, log := NULL]

    components = fs::path_file(file) |>
      fs::path_ext_remove() |>
      stringi::stri_split_fixed(pattern = "__")
    components = components[[1]]
    names(components) = c("tune_measure", "learner_id_long", "task_id", "time_epoch", "iter_hash")

    data.table::data.table(t(components),
               archive = list(archive),
               warnings_sum = sum(archive$warnings),
               errors_sum = sum(archive$errors))
  }))

  archives = archives[learners, on = "learner_id_long"]
  archives = archives[!is.na(tune_measure), ]
  archives[, learner_id_long := NULL]

  archives

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
    keep_columns = c("job.id", "repl", "tags", "task_id", "learner_id", "log.file", "job.name")
    ) {
  alljobs = unwrap(getJobTable(reg = reg), c("prob.pars", "algo.pars"))
  checkmate::assert_data_table(alljobs, min.rows = 1)

  alljobs = alljobs[, keep_columns, with = FALSE]

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

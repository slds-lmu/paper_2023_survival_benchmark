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
    Par = list(learner = "surv.parametric", params = 1),
    Flex = list(learner = "surv.flexible", params = 1),
    RFSRC = list(learner = "surv.rfsrc", params = 5),
    RAN = list(learner = "surv.ranger", params = 5),
    CIF = list(learner = "surv.cforest", params = 5),
    ORSF = list(learner = "surv.aorsf", params = 2),
    RRT = list(learner = "surv.rpart", params = 1),
    MBO = list(learner = "surv.mboost", params = 4),
    CoxB = list(learner = "surv.cv_coxboost", .encode = TRUE, params = 0, internal_cv = TRUE),
    XGB = list(learner = "surv.xgboost", .encode = TRUE, params = 6),
    SSVM = list(learner = "surv.svm", .encode = TRUE, .scale = TRUE, params = 4)
  ) |>
    lapply(data.table::as.data.table) |>
    data.table::rbindlist(fill = TRUE, idcol = TRUE) |>
    setNames(c("learner_id", "learner_id_long", "params", "encode", "internal_cv", "scale")) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(is.na(.x), FALSE, .x)))

  lrnlist |>
    dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(.x, "Yes", "No"))) |>
    write.csv(file = path, row.names = FALSE)

  lrnlist
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
    sizes = vapply(unlist(done), \(x) {
      batchtools::loadResult(x) |>
        pryr::object_size() |>
        as.numeric()
    }, FUN.VALUE = 1)
    done[, size_bytes := prettyunits::pretty_bytes(bytes = sizes)]
    done[, size := sizes / 1024^2][]
  } else {
    message("Don't know yet.")
  }
}


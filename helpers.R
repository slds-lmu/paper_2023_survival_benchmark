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

# Ran into issues when this was defined outside in helpers.R
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

# Create Tasks and corresponding instantiated Resamplings -----------------
#
set.seed(conf$seed)

files = fs::dir_ls(here::here("datasets"), regexp = "\\.rds$")
names = fs::path_ext_remove(fs::path_file(files))
tasks = resamplings = mlr3misc::named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  cli::cli_alert_warning("Using {.val {conf$outer_eval$resampling}} outer resampling!")
  resampling_dir = here::here("resamplings", conf$outer_eval$resampling)
  resampling_csv = fs::path(resampling_dir, names[[i]], ext = "csv")

  # If there is a stored resampling already, use a reconstructed version using the CSV file
  if (fs::file_exists(resampling_csv)) {

    cli::cli_alert_info("Recreating resampling from {.file {fs::path_rel(resampling_csv)}}")
    resampling = create_resampling_from_csv(task, resampling_dir = resampling_dir)

  } else {
    # Otherwise create a new resampling and store it
    cli::cli_alert_info("Creating new resampling for {.val {names[[i]]}}")

    # Make number of folds dependent on number of observations in smallest tasks
    folds = min(floor(task$nrow / conf$outer_eval$min_obs), conf$outer_eval$folds)

    if (conf$outer_eval$repeats == "auto") {

      repeats = data.table::fcase(
        task$nrow < 500, 10,
        task$nrow >= 500 & task$nrow < 5000, 5,
        task$nrow > 5000, 1
      )
      cli::cli_alert_info("Adjusting number of repeats to {.val {repeats}} based on number of observations in task")
    } else {
      repeats = conf$outer_eval$repeats
    }

    resampling = switch(
      conf$outer_eval$resampling,
      "cv"          = rsmp("cv", folds = folds),
      "repeated_cv" = rsmp("repeated_cv", folds = folds, repeats = repeats),
      "holdout"     = rsmp("holdout", ratio = conf$outer_eval$ratio)
    )

    resampling$instantiate(task)

    stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= conf$outer_eval$min_obs))
    save_resampling(resampling, task, resampling_dir = resampling_dir)
    rm(folds, repeats)
  }

  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, resampling)
}

# Only write task metadata table in production-like context, not when debugging
if (config::is_active("production") | config::is_active("trial")) {
  tasktab = save_tasktab(tasks)
}

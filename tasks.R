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
    
    if (conf$outer_eval$resampling == "repeated_cv") {
      if (conf$outer_eval$repeats == "auto") {
        
        # Make number of repeats depending on number of events in dataset (sum(task$status()))
        # For less than 500 events -> 10 repeats
        # between 500 and 2500 events -> 5 repeats
        # over 2500 events: 1 repeat (i.e. only 1 iteration)
        num_events = sum(task$status())
        
        repeats = data.table::fcase(
          num_events < 500, 10,
          num_events >= 500 & num_events < 2500, 5,
          num_events > 2500, 1
        )
        cli::cli_alert_info("Adjusting number of repeats: {.val {num_events}} events -> {.val {repeats}} repeats")
      } else {
        cli::cli_alert_info("Using fixed number of repeats: {.val {num_events}}")
      }
    }
    repeats = conf$outer_eval$repeats
    
    resampling = switch(
      conf$outer_eval$resampling,
      "cv"          = rsmp("cv", folds = folds),
      "repeated_cv" = rsmp("repeated_cv", folds = folds, repeats = repeats),
      "holdout"     = rsmp("holdout", ratio = conf$outer_eval$ratio)
    )
    
    resampling$instantiate(task)
    
    stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= conf$outer_eval$min_obs))
    if (config::is_active("production") | config::is_active("trial")) {
      # Only storing resamplings under (close to) final conditions
      save_resampling(resampling, task, resampling_dir = resampling_dir)
    }
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

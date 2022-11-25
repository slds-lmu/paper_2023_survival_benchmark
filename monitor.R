#! /usr/bin/env Rscript
# monitor current batchmark status
# (Script salvaged from other project, will need further adapting)

library(batchtools)
library(data.table)

root = here::here()
source(file.path(root, "settings.R"))

# Never load writeable while stuff is running
loadRegistry(reg_dir, writeable = FALSE)

# Per-param groupings
params <- c("learner_id", "measure", "task_id")

count_by <- function(ids, params) {
  dt <- unwrap(getJobTable(ids))
  # Clunkily add measure to job list. Stored in tags, tag column 
  dt <- ljoin(dt, getJobTags(ids))
  dt[, measure := tags]
  setnames(dt, "tags", "measure")
  
  if (nrow(dt) > 0) {
    dt[, .(count = .N), by = params][]
  }
}

# Status ----------------------------------------------------------------------------------------------------------
cli::cli_h1("Status")
getStatus()

cat("\n")

# Running -----------------------------------------------------------------
cli::cli_h1("Running")
count_by(findRunning(), params = params)

# Done --------------------------------------------------------------------
cli::cli_h1("Done")
count_by(findDone(), params = params)

cat("\n")

# Expired -----------
cli::cli_h1("Expired")
count_by(findExpired(), params = params)

cat("\n")

# Error'd -----------------------------------------------------------------
cli::cli_h1("Errors")

if (nrow(findErrors()) > 0) {
  print(count_by(findErrors(), params = params))
  
  cat("\n")
  dterrors <- unwrap(getJobTable(findErrors()))
  # Clunkily add measure to job list. Stored in tags, tag column 
  dterrors <- ljoin(dterrors, getJobTags(findErrors()))
  dterrors[, measure := tags]
  setnames(dterrors, "tags", "measure")
  dterrors[, .(job.id, learner_id, task_id, measure, error)] |>
    glue::glue_data(
      "- Learner `{learner_id}` on task `{task_id}` with measure `{measure}`: `{error}`\n\n"
    )
}

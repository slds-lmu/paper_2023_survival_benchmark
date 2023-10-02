#! /usr/bin/env Rscript
# monitor current status
#

library(batchtools)

reg_dir <- here::here("registry")
reg <- suppressMessages(loadRegistry(reg_dir, writeable = FALSE, make.default = FALSE))

job_vars <- c("job.id", "time.running", "task_id", "learner_id")

cli::cli_h1("Current Status: {reg_dir}")
print(getStatus(reg = reg))
cat("\n")

# Running -----------------------------------------------------------------
tbl_running <- unwrap(getJobTable(findRunning(reg = reg), reg = reg))
if (nrow(tbl_running) > 0) {
  cli::cli_h2("Running: {reg_dir}")
  tbl_running[, ..job_vars]
  print(tbl_running[, .(count = .N), by = "learner_id"])
}

# Done --------------------------------------------------------------------
tbl_done <- unwrap(getJobTable(findDone(reg = reg), reg = reg))
if (nrow(tbl_done) > 0) {
  cli::cli_h2("Done: {reg_dir}")
  tbl_done <- tbl_done[, ..job_vars]
  print(tbl_done[, .(count = .N), by = "learner_id"])
}

cat("\n")

# Expired -----------
tbl_expired <- unwrap(getJobTable(findExpired(reg = reg), reg = reg))
if (nrow(tbl_expired) > 0) {
  cli::cli_h2("Expired: {reg_dir}")
  tbl_expired <- tbl_expired[, ..job_vars]
  print(tbl_expired[, .(count = .N), by = "learner_id"])
}

cat("\n")

# Error'd -----------------------------------------------------------------
tbl_errors <- unwrap(getJobTable(findErrors(reg = reg), reg = reg))
if (nrow(tbl_errors) > 0) {
  cli::cli_h2("Errors: {reg_dir}")
  tbl_errors <- tbl_errors[, ..job_vars]
  print(tbl_errors[, .(count = .N), by = "learner_id"])

  print(getErrorMessages(findErrors(reg = reg), reg = reg)[, "message"][, .(count = .N, message = stringr::str_squish(message)), by = "message"])

}


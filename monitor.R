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
  cat("\n")
  print(tbl_running[, .(count = .N), by = "task_id"])
}

# Done --------------------------------------------------------------------
tbl_done <- unwrap(getJobTable(findDone(reg = reg), reg = reg))
if (nrow(tbl_done) > 0) {
  cli::cli_h2("Done: {reg_dir}")
  tbl_done <- tbl_done[, ..job_vars]
  print(tbl_done[, .(count = .N), by = "learner_id"])
  cat("\n")
  print(tbl_done[, .(count = .N), by = "task_id"])
}

cat("\n")

# Expired -----------
tbl_expired <- unwrap(getJobTable(findExpired(reg = reg), reg = reg))
if (nrow(tbl_expired) > 0) {
  cli::cli_h2("Expired: {reg_dir}")
  tbl_expired <- tbl_expired[, ..job_vars]
  print(tbl_expired[, .(count = .N), by = "learner_id"])
  print(tbl_expired[, .(count = .N), by = "task_id"])
}

cat("\n")

# Error'd -----------------------------------------------------------------
tbl_errors <- unwrap(getJobTable(findErrors(reg = reg), reg = reg))
if (nrow(tbl_errors) > 0) {
  cli::cli_h2("Errors: {reg_dir}")
  tbl_errors <- tbl_errors[, ..job_vars]
  print(tbl_errors[, .(count = .N), by = "learner_id"])
  cat("\n")
  print(tbl_errors[, .(count = .N), by = "task_id"])

  errmsg <- getErrorMessages(findErrors(reg = reg), reg = reg)[, "message"]
  errmsg <- errmsg[, .(count = .N), by = "message"]
  errmsg$message <- stringr::str_remove(errmsg$message, "Error : Task ")
  errmsg$message <- stringr::str_remove(errmsg$message, ", but learner")
  errmsg$message <- stringr::str_replace(errmsg$message, "has missing values in column\\(s\\)", "miss:")
  errmsg$message <- stringr::str_replace_all(errmsg$message, "'Center\\.", "C")
  errmsg$message <- stringr::str_remove(errmsg$message, "surv\\.\\w+'s ")
  errmsg$message <- stringr::str_remove(errmsg$message, "does not support this\\nThis happened PipeOp ")
  errmsg$message <- stringr::str_trunc(errmsg$message, 90)

  print(errmsg)

}


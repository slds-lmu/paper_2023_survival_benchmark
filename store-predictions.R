# Take individual batchtools result files and extract only the prediction objects
# Storing them aggregated by learner/tuning measure alongside other results
library(batchtools)
library(mlr3)
library(data.table)

reg <- loadRegistry(conf$reg_dir, writeable = FALSE, work.dir = here::here())

tab <- collect_job_table(
  keep_columns = c("job.id", "repl", "tags", "task_id", "learner_id", "time.running", "mem.used"),
  resource_est_file = ""
)
data.table::setkey(tab, job.id)

ensure_directory(fs::path(conf$result_path, "predictions"))

future::plan("multisession")
subgrid = unique(grid[, .(learner_id, measure)])

invisible(future.apply::future_mapply(
  FUN = \(learn, meas) {
    cli::cli_alert_info(
      "Saving predictions for {.val {learn}} tuned on {.val {meas}}"
    )
    tmp = grid[learner_id == learn & measure == meas]
    tmp[,
      pred := lapply(job.id, \(i) {
        as_prediction(loadResult(i, reg = reg)$prediction$test)
      })
    ]
    name = glue::glue("predictions_{meas}_{learn}")
    path <- fs::path(conf$result_path, "predictions", name, ext = "rds")
    saveRDS(tmp, path)
    return(NULL)
  },
  subgrid$learner_id,
  subgrid$measure,
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE
))

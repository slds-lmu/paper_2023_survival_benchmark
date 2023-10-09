root = here::here()
source(file.path(root, "settings.R"))

library("batchtools")
library("mlr3batchmark")
library(ggplot2)

reg = loadRegistry(reg_dir, writeable = FALSE)


alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id, time.queued, time.running)]
alljobs = alljobs[job.id < 6837, ]
alljobs[, time.running.hms := hms::hms(seconds = as.numeric(time.running, units = "secs"))]
tasktab = readRDS(here::here("tasktab.rds"))
alljobs = ljoin(alljobs, tasktab, by = "task_id")

# Estimate runtime only on selected variables (avoid time.queued, repl)
bt_est = alljobs |>
  dplyr::select(job.id, task_id, n, p, tags, learner_id) |>
  batchtools::estimateRuntimes()
print(bt_est)

alljobs |>
  dplyr::group_by(repl) |>
  dplyr::summarize(
    total = dplyr::n(),
    done = sum(!is.na(time.running)),
    perc = round(100 * done/total, 2)
  ) |>
  print()

if (interactive()) {
  ggplot(alljobs, aes(
    y = reorder(task_id, dimrank),
    x = time.running.hms),
    fill = learner_id
  ) +
    geom_boxplot() +
    geom_vline(xintercept = hms::hms(hours = 12) * 1:2) +
    scale_x_time(breaks = (3600 * 6) * 1:12) +
    labs(
      title = "Job Runtime",
      subtitle = sprintf("based on %i / %i jobs (%s%%)", sum(!is.na(alljobs$time.running.hms)), nrow(alljobs),
                         round(100 * sum(!is.na(alljobs$time.running.hms))/nrow(alljobs), 1))
    ) +
    theme_minimal()
}

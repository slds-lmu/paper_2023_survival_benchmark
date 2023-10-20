root = here::here()
source(file.path(root, "settings_runtime_est.R"))

library("batchtools")
library("mlr3batchmark")
library(ggplot2)
library(data.table)

reg = loadRegistry(reg_dir, writeable = FALSE)


alljobs = unwrap(getJobTable())[, .(job.id, repl, tags, task_id, learner_id, time.queued, time.running, mem.used, error)]
#tasktab = readRDS(here::here("tasktab.rds"))
tasktab = read.csv(here::here("tasktab.csv"))
alljobs = ljoin(alljobs, tasktab, by = "task_id")
setkey(alljobs, job.id)

# Estimate runtime only on selected variables (avoid time.queued, repl)
bt_est = alljobs |>
  dplyr::select(job.id, task_id, learner_id, tags) |>
  batchtools::estimateRuntimes()
print(bt_est)

alljobs = alljobs[bt_est$runtimes]
alljobs[, time.running.hms := hms::hms(seconds = as.numeric(runtime, units = "secs"))]



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
    y = reorder(learner_id, runtime),
    x = runtime,
    fill = type
  )) +
    geom_boxplot() +
    geom_vline(xintercept = hms::hms(hours = 12) * 1:2) +
    scale_x_time(breaks = (3600 * 6) * 1:12) +
    labs(
      title = "Job Runtime: Estimated & Observed",
      subtitle = sprintf("based on %i / %i jobs (%s%%)", sum(!is.na(alljobs$time.running.hms)), nrow(alljobs),
                         round(100 * sum(!is.na(alljobs$time.running.hms))/nrow(alljobs), 1)),
      caption = "Ommiting 10 smallest tasks"
    ) +
    theme_minimal() +
    theme(legend.position = "top")

  ggplot(alljobs[dimrank > 10,], aes(
    y = reorder(task_id, dimrank),
    x = runtime,
    fill = type
  )) +
    geom_boxplot() +
    geom_vline(xintercept = hms::hms(hours = 12) * 1:2) +
    scale_x_time(breaks = (3600 * 6) * 1:12) +
    labs(
      title = "Job Runtime: Estimated & Observed",
      subtitle = sprintf("based on %i / %i jobs (%s%%)", sum(!is.na(alljobs$time.running.hms)), nrow(alljobs),
                         round(100 * sum(!is.na(alljobs$time.running.hms))/nrow(alljobs), 1)),
      caption = "Ommiting 10 smallest tasks"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
}



runtimes <- alljobs |>
  dplyr::select(
    learner_id, task_id,
    #n, p, n_uniq_t,
    tags,
    runtime,
    time.running.hms,
    type,
  ) |>
  dplyr::arrange(dplyr::desc(runtime))

runtimes |>
  #dplyr::select(-runtime) |>
  head(40) |>
  kableExtra::kbl() |>
  kableExtra::kable_styling()

summarize_runtime <- function(xdf, by = "task_id") {
  xdf |>
    dplyr::filter(type == "observed") |>
    dplyr::summarise(
      min = min(.data[["runtime"]], na.rm = TRUE),
      q25 = quantile(.data[["runtime"]], prob = 0.25),
      median_time = median(.data[["runtime"]], na.rm = TRUE),
      #sd_sec = sd(time.running.hms, na.rm = TRUE),
      q75 = quantile(.data[["runtime"]], prob = 0.75),
      max = max(.data[["runtime"]], na.rm = TRUE),
      n_runs = dplyr::n(),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(dplyr::across(min:max, \(x) hms::hms(seconds = round(x, 1)))) |>
    dplyr::arrange(dplyr::desc(.data[["q75"]])) |>
    kableExtra::kbl() |>
    kableExtra::kable_styling()
}


summarize_runtime(runtimes, "task_id")
summarize_runtime(runtimes, "learner_id")
summarize_runtime(runtimes, "tags")

# Memory ------------------------------------------------------------------

alljobs |>
  dplyr::group_by(learner_id) |>
  dplyr::count(is.na(mem.used))

alljobs[is.na(mem.used), c("job.id", "learner_id", "task_id", "tags")]
alljobs[!is.na(mem.used), c("job.id", "learner_id", "task_id", "tags")]


x <- unique(alljobs[!is.na(mem.used)]$learner_id)
setdiff(unique(alljobs$learner_id), x)

alljobs |>
  dplyr::filter(!is.na(mem.used)) |>
  dplyr::group_by(learner_id, task_id) |>
  dplyr::summarize(
    mem_median = median(mem.used, na.rm = TRUE),
    n_measurements = dplyr::n()
  ) |>
  dplyr::arrange(dplyr::desc(mem_median)) |>
  print()

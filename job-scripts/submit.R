# Partition memory in MiB per node: RAM per node per CPU core
mem_beartooth = floor(1024 * (256 / 56))
mem_teton = floor(1024 * (128 / 32))
mem_mb = floor(1024 * (1024 / 96))
#conf = config::get(config = "production")
library("batchtools")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(conf$reg_dir, writeable = TRUE)

# Expecting 576 task (32) x learner (19) combinations, 5 outer folds (except "veteran"), 2 tuning measures
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
tab = collect_job_table(reg = reg)

# tab[is.na(est_total_hours), ]
# tab[is.na(est_mem_mb), ]

# Roughly categorize in alignment qith QoS to make submission easier
# Chunk shortest together to save overhead
tab[,
  time_cat := data.table::fcase(
    est_total_hours <= 1,
    "shortest",
    est_total_hours <= 10,
    "fast",
    est_total_hours <= 2.5 * 24,
    "normal",
    est_total_hours < max(est_total_hours),
    "long",
    est_total_hours == max(est_total_hours),
    "maximum"
  )
]

tab[, .(.N), by = "time_cat"]

# Shortest jobs ----------------------------------------------------------
# These are so small that it's probably faster to chunk in "normal" qos
jobs_shortest = tab[time_cat == "shortest"]
jobs_shortest[, chunk := binpack(est_total_hours, chunk.size = 2 * 24)]
jobs_shortest[, list(hours = sum(est_total_hours), mem = mean(est_mem_mb), count = .N), by = chunk]

jobs_shortest |>
  submitJobs(
    resources = list(
      qos = "normal",
      walltime = 3 * 24 * 3600,
      memory = mem_teton,
      partition = "teton"
    )
  )

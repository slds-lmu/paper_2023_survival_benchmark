# Partition memory in MiB per node: RAM per node per CPU core
mem_beartooth = floor(1024 * (256 / 56))
mem_teton = floor(1024 * (128 / 32))
mem_mb = floor(1024 * (1024 / 96))
#conf = config::get(config = "production")
library("batchtools")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(conf$reg_dir, writeable = TRUE)

# Expecting 646 total
print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
tab = collect_job_table(reg = reg, optional_columns = c("batch.id", "comment", "memory", "walltime", "time.running"))

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
    est_total_hours < 160,
    "long",
    est_total_hours >= 160,
    "maximum"
  )
]

tab[, .(n = .N, min_time_h = min(est_total_hours), max_time_h = max(est_total_hours)), by = "time_cat"]

expired = findExpired()
if (nrow(expired) > 0) {
  expired = ijoin(tab, expired)[, .(job.id, task_id, learner_id, time_cat, est_total_hours, walltime, memory, time.running)]
  expired[, time.running := as.numeric(time.running)]
  data.table::setnames(expired, "memory", "previous_memory")
  data.table::setnames(expired, "walltime", "previous_walltime")

  expired_oom = ijoin(expired, grepLogs(expired, pattern = "OOM"))

  expired[, .(n = .N), by = .(time_cat)]
 
 expired[time_cat == "normal", ]
}


errored = findErrors()
if (nrow(errored) > 0) {
  errored = ijoin(tab, errored)[, .(job.id, task_id, learner_id, time_cat, est_total_hours, walltime, memory, time.running)]
  errored[, time.running := as.numeric(time.running)]
  data.table::setnames(errored, "memory", "previous_memory")
  data.table::setnames(errored, "walltime", "previous_walltime")

  errored = ijoin(errored, grepLogs(errored, pattern = "OOM"))

  errored[, .(n = .N), by = .(time_cat)]
 
 errored[time_cat == "normal", ]
}

# Shortest jobs ----------------------------------------------------------
# These are so small that it's probably faster to chunk in "normal" qos
jobs_shortest = tab[time_cat == "shortest"]
jobs_shortest[, chunk := binpack(est_total_hours, chunk.size = 2 * 24)]
jobs_shortest[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N),
  by = chunk
]

jobs_shortest[, .(job.id, chunk)] |>
  ijoin(findNotSubmitted()) |>
  submitJobs(
    resources = list(
      qos = "normal",
      walltime = 3 * 24 * 3600,
      memory = mem_teton,
      partition = "teton",
      comment = "shortest-chunked"
    )
  )

# Fast jobs ----------------------------------------------------------
# Fast QoS -> 12 hours max
jobs_fast = tab[time_cat == "fast"]
jobs_fast[, chunk := binpack(est_total_hours, chunk.size = 11)]
jobs_fast[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N),
  by = chunk
]

jobs_fast[, .(job.id, chunk)] |>
  ijoin(findNotSubmitted()) |>
  submitJobs(
    resources = list(
      qos = "fast",
      walltime = 12 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "fast-chunked"
    )
  )

expired_fast = expired[time_cat == "fast", ]
expired_fast[, chunk := binpack(est_total_hours, chunk.size = 10)]
expired_fast[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N),
  by = chunk
]

expired_fast[, .(job.id, chunk)] |>
  ajoin(findRunning()) |>
  submitJobs(
    resources = list(
      qos = "fast",
      walltime = 12 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "fast-chunked-resubmit"
    )
  )

grepLogs(expired_fast, pattern = "OOM") |>
    submitJobs(
    resources = list(
      qos = "normal",
      walltime = 72 * 3600,
      memory = 2 * mem_mb,
      partition = "mb",
      comment = "resubmit-oom-normal"
    )
  )

# Normal jobs ----------------------------------------------------------
# Normal QoS -> 3 days (72 hours) max
jobs_normal = tab[time_cat == "normal"]
jobs_normal[, chunk := binpack(est_total_hours, chunk.size = 60)]
jobs_normal[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N),
  by = chunk
]

jobs_normal[chunk <= 1600, .(job.id, chunk)] |>
  ijoin(findNotSubmitted()) |>
  submitJobs(
    resources = list(
      qos = "normal",
      walltime = 3 * 24 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "normal-chunked"
    )
  )

jobs_normal[chunk <= 1800, .(job.id, chunk)] |>
  ijoin(findNotSubmitted()) |>
  submitJobs(
    resources = list(
      qos = "normal",
      walltime = 3 * 24 * 3600,
      memory = mem_beartooth,
      partition = "beartooth",
      comment = "normal-chunked"
    )
  )

jobs_normal[, .(job.id)] |>
  ijoin(expired) |>
  dplyr::mutate(chunk = binpack(est_total_hours, chunk.size = 60)) |>
  submitJobs(
    resources = list(
      qos = "normal",
      walltime = 3 * 24 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "normal-resubmit-errs"
    )
  )


# Long jobs ----------------------------------------------------------
# Long QoS -> 7 days (168 hours) max
# This is where chunking stops being very useful but still could be.
jobs_long = tab[time_cat == "long"]
jobs_long[, chunk := binpack(est_total_hours, chunk.size = 160)]
jobs_long[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N),
  by = chunk
]

jobs_long[chunk <= 300, .(job.id, chunk)] |>
  ijoin(findNotSubmitted()) |>
  submitJobs(
    resources = list(
      qos = "long",
      walltime = 7 * 24 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "long-chunked"
    )
  )

# Longest / maximum time jobs ----------------------------------------------------------
# Long QoS -> 7 days (168 hours) max
# These are the ones most likely to run into walltime kill and starting them at all seems... optimistic.
jobs_maximum = tab[time_cat == "maximum"]
jobs_maximum[,
  list(hours = sum(est_total_hours), min_time_h = min(est_total_hours), max_time_h = max(est_total_hours), count = .N)
]

jobs_maximum[, .(job.id)] |>
  ijoin(findNotSubmitted()) |>
  head(40) |>
  submitJobs(
    resources = list(
      qos = "long",
      walltime = 7 * 24 * 3600,
      memory = mem_mb,
      partition = "mb",
      comment = "maximum"
    )
  )

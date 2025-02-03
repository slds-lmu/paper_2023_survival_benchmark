# Submitting slowest jobs at faster partition -------------------------------------------------
# From previous run -- can be used as basis for subsequent attempts

jobs_untuned[chunk %in% chunks_untuned[count == 1, chunk]] |>
  submitJobs(resources = list(partition = "teton", memory = mem_teton))

jobs_harrell[chunk %in% chunks_harrell[count == 1, chunk]] |>
  submitJobs(resources = list(partition = "teton", memory = mem_teton))

jobs_isbs[chunk %in% chunks_isbs[count == 1, chunk]] |>
  submitJobs(resources = list(partition = "teton", memory = mem_teton))

# Submitting bulk of jobs ---------------------------------------------------------------------

jobs_untuned[!(chunk %in% chunks_untuned[count == 1, chunk])] |>
  submitJobs()

jobs_harrell[!(chunk %in% chunks_harrell[count == 1, chunk])] |>
  submitJobs()

jobs_isbs[!(chunk %in% chunks_isbs[count == 1, chunk])] |>
  submitJobs()

# Checkup -------------------------------------------------------------------------------------

jobs_untuned[!(chunk %in% chunks_untuned[count == 1, chunk])] |>
  findDone() |>
  nrow()

jobs_untuned[chunk %in% chunks_untuned[count == 1, chunk]] |>
  findDone() |>
  nrow()

jobs_harrell[!(chunk %in% chunks_harrell[count == 1, chunk])] |>
  findDone() |>
  nrow()

jobs_isbs[!(chunk %in% chunks_isbs[count == 1, chunk])] |>
  findDone() |>
  nrow()

# Resubmitting XGBCox -------------------------------------------------------------------------

# Deleting tuning archive files of obsolete jobs
tictoc::tic()
archives = reassemble_archives(keep_logs = FALSE)
tictoc::toc()

xgbcoxfiles = archives[learner_id == "XGBCox", file]
fs::file_delete(xgbcoxfiles)
any(fs::file_exists(xgbcoxfiles))


# Just a few for testing
xgbtest = alljobs[learner_id == "XGBCox" & uniq_t_rank <= 30 & measure == "harrell_c", ]
findNotSubmitted(xgbtest)
findQueued(xgbtest)

submitJobs(
  xgbtest,
  resources = list(partition = "teton")
)

nrow(findRunning(xgbtest)) / nrow(xgbtest)
nrow(findDone(xgbtest)) / nrow(xgbtest)
nrow(findNotDone(xgbtest)) / nrow(xgbtest)

xgbrest = alljobs[learner_id == "XGBCox" & uniq_t_rank <= 30, ]
xgbrest[, chunk := lpt(n_uniq_t, n.chunks = 15)]
xgbrest[, memory := NULL]
xgbrest[, comment := "proba-xgb-resubmit"]

submitJobs(
  xgbrest,
  resources = list(partition = "teton", memory = 4096)
)

xgbrest_large = alljobs[learner_id == "XGBCox" & uniq_t_rank > 30, ]
xgbrest_large[, memory := 5 * 4096]
xgbrest_large[, comment := "proba-xgb-resubmit-large"]

submitJobs(
  xgbrest_large,
  resources = list(partition = "teton")
)

# Some larger jobs
submitJobs(
  findNotSubmitted(),
  resources = list(partition = "teton", memory = 5 * 4096, comment = "proba-resubmit-large")
)

# Resubmitting OOM jobs -----------------------------------------------------------------------

# via sinfo -N -l
mem_node_beartooth = 257000
mem_node_tknl = 384000
mem_node_teton = 192000

# findExpired() should suffice
oom_ids = grepLogs(rbind(findDone(), findExpired()), pattern = "oom_kill", fixed = TRUE)
oom_jobs = alljobs[oom_ids]
oom_jobs[, matches := NULL]
oom_jobs[, memory_prev := memory]
oom_jobs[, memory := NULL]
oom_jobs

any(oom_jobs$job.id %in% findQueued()[[1]])

oom_jobs |>
  submitJobs(resources = list(
    partition = "teton-knl",
    memory = 384000 / 12,
    comment = "proba-oom-resubmit-oom"
  ))

oom_jobs |>
  submitJobs(resources = list(
    partition = "teton-hugemem",
    memory = 128 * 1024,
    comment = "proba-oom-flex"
  ))

oom_jobs |>
  submitJobs(resources = list(
    partition = "teton-hugemem",
    memory = 128 * 1024,
    comment = "proba-oom-huge"
  ))

oom_jobs |>
  submitJobs(resources = list(
    partition = "beartooth-hugemem",
    memory = 160 * 1024,
    comment = "proba-oom-huge"
  ))

oom_jobs |>
  submitJobs(resources = list(
    partition = "teton",
    memory = 192000 / 10,
    comment = "proba-oom-resubmit-oom"
  ))

togo = alljobs[findNotSubmitted()]
togo = togo[task_id %in% c("hdfail", "child") & learner_id != "XGBCox", ]

togo |>
  submitJobs(resources = list(
    partition = "teton",
    memory = 192000 / 8
  ))


alljobs = collect_job_table(reg = reg, keep_columns =
  c("job.id", "repl", "tags", "task_id", "learner_id", "log.file", "job.name", "memory"))



# Resubmit SSVM? ------------------------------------------------------------------------------


ssvm_jobs = alljobs[task_id == "veteran" & learner_id == "SSVM" & measure == "harrell_c"]

ssvm_jobs |>
submitJobs(resources = list(
  partition = "teton",
  memory = 192000 / 16,
  comment = "proba-resubmit-ssvm-1"
))

##


togo = alljobs[findNotSubmitted()]
togo = togo[task_id %in% c("hdfail", "child"), ]

togo |>
  submitJobs(resources = list(
    partition = "teton",
    memory = 192000 / 8
  ))


togo[dimrank < 25 & learner_id != "SSVM", ] |>
  submitJobs(resources = list(
    partition = "teton",
    memory = 192000 / 16
  ))

togo[dimrank < 25 & learner_id == "SSVM", ] |>
  submitJobs(resources = list(
    partition = "teton-hugemem",
    memory = 32 * 1024
  ))

togo[dimrank >= 25 & learner_id == "SSVM", ] |>
  submitJobs(resources = list(
    partition = "teton-hugemem",
    memory = 64 * 1024
  ))

togo[dimrank >= 25 & dimrank < 30 & learner_id != "SSVM", ] |>
  submitJobs(resources = list(
    partition = "teton",
    memory = 192000 / 10
  ))

togo[dimrank > 30 & learner_id != "SSVM", ] |>
  submitJobs(resources = list(
    partition = "teton-hugemem",
    memory = 64 * 1024
  ))

findNotSubmitted() |>
submitJobs(resources = list(
  partition = "teton-hugemem",
  memory = 16 * 1024
))

alljobs = collect_job_table()
togo = alljobs[job.id %in% findNotSubmitted()[[1]]]
togo$memory = NULL
togo[, chunk := chunk(job.id, chunk.size = 100)]
submitJobs(
  togo,
  resources = list(partition = "teton", memory = 4096, comment = "proba-resubmit")
)

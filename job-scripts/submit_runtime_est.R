library(batchtools)

if (Sys.getenv('R_CONFIG_ACTIVE', 'default') != "runtime") {
  cli::cli_warn("Expecting profile {.val runtime} but profile set to {.val {conf_profile}}")
}

reg = loadRegistry(conf$reg_dir, writeable = TRUE)
# Get all jobs that belong to a first repl, i.e. just one outer eval fold
# While ISBS is slower and would be more conservative estimate,
# not all learners can be tuned with it, and doing harrell_c only is then easiest and should be okay
# We esclude "isbs" such that "harrell_c" and "harrell_c,isbs" (== "untuned" learners) are left
tab = collect_job_table(reg = reg)
tab = tab[repl == 1 & measure != "isbs", ]

getStatus()

# Sanity checking counts:
nrow(tab) == nrow(tab[, .(n = .N), by = .(learner_id, task_id, measure)])
jobs_per_learner = tab[, .(n = .N), by = .(learner_id)]
(jobs_per_learner_n = unique(jobs_per_learner$n))
jobs_per_task = tab[, .(n = .N), by = .(task_id)]
(jobs_per_task_n = unique(jobs_per_task$n))
stopifnot(nrow(jobs_per_learner) == jobs_per_task_n)
stopifnot(nrow(jobs_per_task) == jobs_per_learner_n)

tab[learner_id %in% c("KM", "NA", "CPH")] |>
  findNotSubmitted() |>
  submitJobs(
    resources = list(
    qos = "fast", walltime = 6 * 3600
    )
  )

tab[uniq_t_rank <= 10] |>
  findNotSubmitted() |>
  submitJobs(
    resources = list(
      qos = "fast", walltime = 12 * 3600
    )
  )

tab[uniq_t_rank > 10 & uniq_t_rank <= 20] |>
  findNotSubmitted() |>
  submitJobs(
    resources = list(
      qos = "normal", walltime = 3 * 24 * 3600,
      memory = 10L * 1024L,
      partition = "mb"
    )
  )

tab[uniq_t_rank > 20] |>
  findNotSubmitted() |>
  submitJobs(
    resources = list(
      qos = "long", walltime = 7 * 24 * 3600,
      memory = 10L * 1024L,
      partition = "mb"
    )
  )

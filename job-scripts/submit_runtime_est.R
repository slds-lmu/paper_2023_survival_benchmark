library(batchtools)

if (Sys.getenv('R_CONFIG_ACTIVE', 'default') != "runtime") {
  cli::cli_warn("Expecting profile {.val runtime} but profile set to {.val {conf_profile}}")
}

reg = loadRegistry(settings$reg_dir, writeable = TRUE)
tab = collect_job_table(reg = reg)

getStatus()

# Get all jobs that belong to a first repl, i.e. just one outer eval fold
# While ISBS is slower and would be more conservative estimate,
# not all learners can be tuned with it, and doing harrell_c only is then easiest and should be okay
# We esclude "isbs" such that "harrell_c" and "harrell_c,isbs" (== "untuned" learners) are left

sample_ids = tab[repl == 1 & measure != "isbs", ]

sample_ids[learner_id %in% c("KM", "NA", "CPH")] |>
  findNotSubmitted() |>
  submitJobs()

sample_ids[uniq_t_rank <= 10] |>
  findNotSubmitted() |>
  submitJobs()

sample_ids[uniq_t_rank > 10 & uniq_t_rank <= 20] |>
  findNotSubmitted() |>
  submitJobs()

sample_ids[uniq_t_rank > 20] |>
  findNotSubmitted() |>
  submitJobs()

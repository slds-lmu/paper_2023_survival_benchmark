library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(conf$reg_dir, writeable = TRUE)
tab = collect_job_table(reg = reg)

tab[, .N, by = .(task_id, measure)]

jobs_untuned = tab[grepl(",", measure), ]
jobs_harrell = tab[measure == "harrell_c", ]
jobs_isbs = tab[measure == "isbs", ]

jobs_harrell[repl == 1] |>
  submitJobs()

sample_ids = tab[uniq_t_rank <= 5, .SD[sample(nrow(.SD), 2)], by = c("task_id", "learner_id", "measure")]
sample_ids |>
  findNotSubmitted() |>
  submitJobs()


sample_ids = jobs_harrell[uniq_t_rank <= 3 & learner_id == "XGBAFT", .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]
sample_ids |>
  findNotSubmitted() |>
  submitJobs()

sample_ids = jobs_harrell[uniq_t_rank <= 3 & learner_id == "XGBCox", .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]



tab[startsWith(learner_id, "MBST") & uniq_t_rank <= 2 & repl <= 3, ] |>
  submitJobs()

findDone()


loadResult(15973)$prediction$test$distr |> head()
loadResult(13481)$prediction$test$distr |> head()




jobs_harrell[repl < 3 & uniq_t_rank <= 10 & learner_id == "SSVM",] |>
  findNotSubmitted() |>
  submitJobs()

jobs_harrell[repl == 1 & uniq_t_rank < 7,] |>
  findNotSubmitted() |>
  submitJobs()

jobs_harrell[uniq_t_rank < 2 & learner_id == "SSVM",] |>
  findNotSubmitted() |>
  submitJobs()

jobs_harrell[uniq_t_rank <= 2 & learner_id == "XGBCox" & repl == 1, ] |>
  findNotSubmitted() |>
  submitJobs()

bmr = reduceResultsBatchmark(findDone())
scores = bmr$score(conditions = TRUE)
aggr = bmr$aggregate(conditions = TRUE)

scores[surv.cindex == 0.5]

tab[learner_id == "XGBCox" & task_id == "grace" & job.id %in% findDone()[[1]]]
tab[job.id %in% 520]

submitJobs(520)

testJob(520)

done = tab[findDone()]
done[, .(task_id, job.id)]

res = loadResult(977)
res$learner_state$model$marshaled$tuning_instance$result$internal_tuned_values


x = bmr$resample_result(9)
x$learner$tuning_instance
x$learner$archive
x$learner$validate
x$learner$instance_args
x$learner$fallback$state
x$learner$fallback$archive
x$learner$state
x$learner$log


res = loadResult(1246)
res$learner_state$log
res$learner_state$validate
res$learner_state$fallback_state

learners$XGBCox$train(tasks$grace)
learners$XGBCox$predict(tasks$grace)
learners$XGBCox$log



cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()

library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(settings$reg_dir, writeable = TRUE)
tab = collect_job_table(reg = reg)

ids = tab[uniq_t_rank <= 15,.SD[sample(nrow(.SD), 2)], by = c("task_id", "learner_id", "measure")]
ids |>
  findNotSubmitted() |>
  submitJobs()


# ssvm ----------------------------------------------------------------------------------------


getLog(446)

tictoc::tic()
testJob(446)
tictoc::toc()

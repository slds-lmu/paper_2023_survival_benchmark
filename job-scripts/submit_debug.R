library("batchtools")
library("mlr3batchmark")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(conf$reg_dir, writeable = TRUE)
tab = collect_job_table(reg = reg)

tab[, .N, by = .(task_id, measure)]

tab = tab[learner_id == "ORSF",]

jobs_harrell = tab[measure == "harrell_c", ]
jobs_isbs = tab[measure == "isbs", ]

jobs_harrell[repl == 1] |>
  submitJobs()

sample_ids = tab[uniq_t_rank <= 5 & repl == 1, ]
sample_ids |>
  findNotSubmitted() |>
  submitJobs()


sample_ids = jobs_harrell[uniq_t_rank <= 3 & learner_id == "XGBAFT", .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]
sample_ids |>
  findNotSubmitted() |>
  submitJobs()

tab |>
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

archives = reassemble_archives(ignore_cache = TRUE)
archives_long = archives[, as.data.table(archive), by = .(learner_id, task_id, tuning_measure, iter_hash, time_epoch, file)]

archives[errors_sum > 0]

names(archives)

names(archives$archive[[1]])

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

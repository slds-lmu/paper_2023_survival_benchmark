library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(batchtools)
library(mlr3batchmark)

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config {.val {Sys.getenv('R_CONFIG_ACTIVE', 'default')}}")
settings = config::get()
reg = loadRegistry(settings$reg_dir, writeable = TRUE)
tab = collect_job_table(reg = reg)

getStatus()

jobs_untuned = tab[grepl(",", measure), ]
jobs_harrell = tab[measure == "harrell_c", ]
jobs_isbs = tab[measure == "isbs", ]

sample_ids = tab[, .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]

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

# Resource estimates

sample_ids = tab[, .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id", "measure")]
sample_ids[, .(n = .N), by = .(task_id, learner_id, measure)]

submitJobs(sample_ids)

# aggr ----------------------------------------------------------------------------------------

tab[findDone()]

bmr = reduceResultsBatchmark(findDone(), store_backends = TRUE, unmarshal = TRUE)
scores = bmr$score(measure = msr("surv.cindex"), conditions = TRUE)
aggr = bmr$aggregate(measure = msr("surv.cindex"), conditions = TRUE)

scores
aggr

bmr$resample_result(5) -> x
x$learners[[1]]$tuning_instance
as.data.table(x$predictions()[[1]])

bmr$learners$learner[[1]]$tuning_instance


res = loadResult(2451)
res$learner_state

res = loadResult(9285)
res$learner_state$log |> View()



library(ggplot2)

scores |>
  ggplot(aes(x = surv.cindex, y = reorder(learner_id, surv.cindex))) +
  facet_wrap(vars(task_id)) +
  geom_boxplot() +
  theme_minimal()


# ssvm ----------------------------------------------------------------------------------------

ssvm = jobs_harrell[learner_id == "SSVM"]

ssvm[uniq_t_rank <= 4] |>
  submitJobs()

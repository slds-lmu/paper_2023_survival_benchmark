library(survival)
library(mlr3pipelines)
library(mlr3proba)

tasks = load_task_data()

ph_check = data.table::rbindlist(
  mlr3misc::imap(tasks, \(task_data, task_id) {
    # cli::cli_alert_info("Computing {.fun cox.zph} for {.val {task_id}}")
    tryCatch(
      {
        # Convert to mlr3 task as in benchmark with same preprocessing
        task = as_task_surv(task_data, target = "time", event = "status", id = task_id)
        # Apply same preprocessing as in benchmark, outputs mlr3 task obj
        preproc = po("collapsefactors", no_collapse_above_prevalence = 0.05, target_level_count = 5) %>>%
          po("encode", method = "treatment") %>>%
          po("removeconstants")
        preproc$train(input = task)
        task_data_processed = preproc$predict(task)[[1]]
        # Fit CPH
        fit = lrn("surv.coxph")$train(task_data_processed)$model
        # Perform test (tests correlation between schoenfeld residuals an time, should be approx. 0)
        # global test requires inversion of covariance matrix, fails for CarpenterFdaData, so we skip it with NA
        zph = cox.zph(fit, global = TRUE, terms = TRUE)
        result_pval = zph$table["GLOBAL", "p"]
        # Could also use task_data_processed$prop_haz(), same approach
        data.table::data.table(task_id = task_id, zph_pval = result_pval, zph = list(zph))
      },
      error = function(msg) {
        # If the test fails we assume PH violation for safety
        return(data.table::data.table(task_id = task_id, zph_pval = NA, zph = NA))
      }
    )
  })
)

ph_check
saveRDS(ph_check, file = fs::path(here::here("tables"), "ph_check", ext = "rds"))


# CarpenterFdaData extracheck

task_data = tasks$CarpenterFdaData
task = as_task_surv(task_data, target = "time", event = "status", id = "CarpenterFdaData")
task_data_processed = preprocess(task)
fit = coxph(
  Surv(time, status) ~ .,
  data = task_data_processed$data(),
  x = TRUE,
  singular.ok = TRUE,
  ties = "efron"
)
zph = cox.zph(fit, global = TRUE, terms = TRUE)
result = zph$table["GLOBAL", ]

sresid <- residuals(fit, type = "scaledsch")
# Test each covariate individually against time
event_times <- task_data_processed$data()[status == 1, time]
feature_corrs = data.table::rbindlist(lapply(task_data_processed$feature_names, \(x) {
  test = cor.test(event_times, sresid[, x], method = "kendall")

  data.frame(
    feature = x,
    statistic = test$estimate,
    p = test$p.value
  )
}))
feature_corrs[order(p)]

p.adjust(feature_corrs$p, method = "BH") |> sort()

event_times <- task_data_processed$data()[status == 1, time]
time_rank <- rank(event_times)
mv_fit <- lm(sresid ~ time_rank)

# Joint test using Pillai's trace or similar
car::Anova(mv_fit, test = "Pillai")

survfit(formula = Surv(time, status) ~ 1, data = as.data.frame(task_data_processed$data())) |> plot()
survfit(formula = Surv(time, status) ~ 1, newdata = as.data.frame(task_data_processed$data())) |> plot()

reformulate(
  c(
    "acutediz",
    "condavg3",
    "deathrt1",
    "demhsmaj",
    "demsnmaj",
    "femdiz01",
    "hcomm",
    "hfloor",
    "hhosleng",
    "hosp01",
    "hospdisc",
    "lethal",
    "mandiz01",
    "natreg",
    "natregsq",
    "orderent",
    "orphdum",
    "peddiz01",
    "prespart",
    "prevgenx",
    "scomm",
    "sfloor",
    "stafcder",
    "vandavg3",
    "wpnoavg3"
  ),
  response = Surv(time, status)
)

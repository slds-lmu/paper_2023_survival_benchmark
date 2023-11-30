root = here::here()
reg_dir = file.path(root, "registry_dcalib_test")

###################################################################################################
### Packages
###################################################################################################
library(batchtools)
library(mlr3)
library(mlr3proba)
library(mlr3batchmark)
library(mlr3benchmark)
library(ggplot2)

reg = loadRegistry(reg_dir, writeable = TRUE, make.default = TRUE)

alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

results_dir <- here::here("tmp")
if (!file.exists(results_dir)) dir.create(results_dir)

###################################################################################################
### Reduce results
###################################################################################################
# Store eval measures for easier retrieval
measures_eval = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  msr("surv.rcll", id = "rcll"),

  msr("surv.graf", id = "graf_proper", proper = TRUE),
  msr("surv.graf", id = "graf_improper", proper = FALSE),

  msr("surv.dcalib", id = "dcalib_inf", truncate = Inf),

  msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  msr("surv.logloss", id = "logloss"),
  msr("surv.calib_alpha", id = "calib")
)
names(measures_eval) = mlr3misc::ids(measures_eval)

unique(alljobs$measure)

if (FALSE) {
  res = vector(mode = "list", length(unique(alljobs$measure)))
  names(res) = unique(alljobs$measure)

  for (meas in unique(alljobs$measure)) {
    ids = alljobs[measure == meas, ]

    tictoc::tic(msg = glue::glue("Reducing results: {meas}"))
    bmr = reduceResultsBatchmark(ids = findDone(ids, reg = reg), store_backends = TRUE, reg = reg)
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Aggregating results: {meas}"))
    aggr = bmr$aggregate(measures = measures_eval, conditions = TRUE)
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Saving results: {meas}"))
    saveRDS(bmr, file = glue::glue("{results_dir}/bmrs_runtime_est_{meas}.rds"))
    tictoc::toc()

    tictoc::tic(msg = glue::glue("Saving aggregated results: {meas}"))
    aggr[, resample_result := NULL]
    saveRDS(aggr, glue::glue("{results_dir}/aggr_runtime_est_{meas}.rds"))
    tictoc::toc()

    res[[meas]] = list(bmr = bmr, aggr = aggr)
  }
}

if (!file.exists("{results_dir}/aggr_combined.rds")) {
  aggr_untuned      = readRDS(here::here(results_dir, "aggr_runtime_est_dcalib_inf,dcalib_trunc,rcll.rds"))
  aggr_dcalib_trunc = readRDS(here::here(results_dir, "aggr_runtime_est_dcalib_trunc.rds"))
  aggr_dcalib_inf   = readRDS(here::here(results_dir, "aggr_runtime_est_dcalib_inf.rds"))
  aggr_rcll         = readRDS(here::here(results_dir, "aggr_runtime_est_rcll.rds"))

  aggr_untuned[, tuning_measure := "none"]
  aggr_dcalib_trunc[, tuning_measure := "dcalib_trunc"]
  aggr_dcalib_inf[, tuning_measure := "dcalib_inf"]
  aggr_rcll[, tuning_measure := "rcll"]

  aggr = data.table::rbindlist(list(aggr_untuned, aggr_dcalib_inf, aggr_dcalib_trunc, aggr_rcll))
  saveRDS(aggr, here::here(results_dir, "aggr_combined.rds"))
} else {
  aggr = readRDS(here::here(results_dir, "aggr_combined.rds"))
}

tictoc::tic("reading bmrs")
bmr_untuned      = readRDS(here::here(results_dir, "bmrs_runtime_est_dcalib_inf,dcalib_trunc,rcll.rds"))
bmr_dcalib_trunc = readRDS(here::here(results_dir, "bmrs_runtime_est_dcalib_trunc.rds"))
bmr_dcalib_inf   = readRDS(here::here(results_dir, "bmrs_runtime_est_dcalib_inf.rds"))
bmr_rcll         = readRDS(here::here(results_dir, "bmrs_runtime_est_rcll.rds"))
tictoc::toc()

bma_dcalib_inf   = as_benchmark_aggr(bmr_dcalib_inf, measures = measures_eval)
bma_dcalib_trunc = as_benchmark_aggr(bmr_dcalib_trunc, measures = measures_eval)
bma_rcll  = as_benchmark_aggr(bmr_rcll, measures = measures_eval)

bmr_rcll_plus = bmr_untuned$combine(bmr_rcll)
bma_rcll_plus  = as_benchmark_aggr(bmr_rcll_plus, measures = measures_eval)

bma_rcll_plus$friedman_posthoc(meas = "graf_proper")
bma_rcll_plus$friedman_posthoc(meas = "rcll")
bma_rcll_plus$friedman_posthoc(meas = "uno_c")
bma_rcll_plus$friedman_posthoc(meas = "dcalib_inf")


bma_dcalib_inf$friedman_posthoc()
bma_dcalib_trunc$friedman_posthoc()
bma_rcll$friedman_posthoc()

for (measure in unique(names(measures_eval))) {
  autoplot(bma_rcll_plus, type = "cd", ratio = 1/5, meas = measure) +
    labs(title = glue::glue("Tuned on RCLL, evaluated on {measure}"))
  ggsave(here::here(results_dir, glue::glue("cd_rcll_eval-{measure}.png")))
}



autoplot(bma_dcalib_inf, type = "cd", ratio = 1/5)
ggsave(here::here(results_dir, "cd_dcalib_inf.png"))

autoplot(bma_dcalib_trunc, type = "cd", ratio = 1/5)
ggsave(here::here(results_dir, "cd_dcalib_trunc.png"))

autoplot(bma_rcll, type = "cd", ratio = 1/5)
ggsave(here::here(results_dir, "cd_rcll.png"))

# Looking at aggr ---------------------------------------------------------


aggr |>
  dplyr::filter(tuning_measure != "none", learner_id == "XGB", task_id == "aids2") |>
  ggplot(aes(y = tuning_measure, x = graf_proper)) +
  facet_grid(rows = vars(learner_id), cols = vars(task_id), scales = "free_x") +
  geom_col() +
  theme_minimal()

aggr |>
  dplyr::filter(learner_id == "XGB", task_id == "aids2") |>
  tidyr::pivot_longer(cols = harrell_c:calib, names_to = "eval_measure", values_to = "score") |>
  ggplot(aes(y = tuning_measure, x = score)) +
  facet_wrap(vars(eval_measure), scales = "free_x", ncol = 1, labeller = label_both) +
  geom_col(alpha = 2/3, color = "gray10") +
  labs(title = "XGB on aids2") +
  theme_minimal()

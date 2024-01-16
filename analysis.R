root = here::here()
source(file.path(root, "helpers.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get()


###################################################################################################
### Packages
###################################################################################################
library(batchtools)
library(mlr3batchmark)
library(mlr3benchmark)
library(mlr3proba)
requireNamespace("mlr3extralearners")
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(ggplot2)

reg_dir = file.path(root, settings$reg_name)
# reg = loadRegistry(reg_dir, writeable = TRUE)

# alljobs = collect_job_table(reg)
result_path = here::here("results")

# Reassembling tuning archives ----------------------------------------------------------------

if (FALSE) {
  archives = reassemble_archives(reg_dir = reg_dir, result_path = result_path)
  archives[errors_sum > 0, c("tune_measure", "task_id", "learner_id", "errors_sum")]
}

# Reducing results ----------------------------------------------------------------------------

# Store eval measures for easier retrieval
measures_eval = get_measures_eval()

collect_results(settings$reg_name, tuning_measure = "harrell_c", measures_eval = measures_eval, result_path = here::here("results"))
collect_results(settings$reg_name, tuning_measure = "rcll", measures_eval = measures_eval, result_path = here::here("results"))

bmr_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bmr_harrell_c.rds"))
aggr_harrell_c = readRDS(fs::path(result_path, settings$reg_name, "aggr_harrell_c.rds"))
bma_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bma_harrell_c.rds"))

# Quick check ---------------------------------------------------------------------------------

bmr_tab = bmr_harrell_c$aggregate(measures = list(), conditions = TRUE)
bmr_tab[errors > 0,][, .(n = .N), by = .(task_id)]
bmr_tab[errors > 0,][, .(n = .N), by = .(learner_id)]


bma_harrell_c$friedman_posthoc(meas = measures_eval$harrell_c$id)

mlr3viz::autoplot(bma_harrell_c, meas = measures_eval$harrell_c$id)
mlr3viz::autoplot(bma_harrell_c, type = "cd", meas = measures_eval$harrell_c$id)
mlr3viz::autoplot(bma_harrell_c, type = "box", meas = measures_eval$harrell_c$id)


aggr_harrell_c |>
  ggplot(aes(x = learner_id, y = harrell_c)) +
  geom_boxplot()

aggr_harrell_c |>
  ggplot(aes(x = learner_id, y = graf_proper)) +
  geom_boxplot()


# resamplings_with_error = aggr_harrell_c[errors > 0, nr]
# bma_harrell_c$resample_result(resamplings_with_error[1])$errors

# aggr_harrell_c[learner_id %in% c("XGBCox", "XGBAFT"), ]

if (FALSE) {
  ggplot(aggr, aes(x = learner_id, y = harrell_c)) +
    facet_wrap(vars(task_id)) +
    geom_boxplot() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme_minimal(base_size = 14)

  scores = bmr_harrell$score(measures_eval$rcll)

  ggplot(scores, aes(x = learner_id, y = rcll)) +
    facet_wrap(vars(task_id)) +
    geom_boxplot() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme_minimal()

}

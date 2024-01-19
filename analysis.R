root = here::here()
source(file.path(root, "helpers.R"))

# Using active config as set per R_CONFIG_ACTIVE env var, see config.yml
# See https://rstudio.github.io/config/articles/config.html
cli::cli_alert_info("Loading config \"{Sys.getenv('R_CONFIG_ACTIVE', 'default')}\"")
settings = config::get(config = "beartooth")


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
measures_eval_ids = mlr3misc::ids(measures_eval)

collect_results(settings$reg_name, tuning_measure = "harrell_c", measures_eval = measures_eval, result_path = here::here("results"))
collect_results(settings$reg_name, tuning_measure = "rcll", measures_eval = measures_eval, result_path = here::here("results"))

bmr_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bmr_harrell_c.rds"))
# aggr_harrell_c = readRDS(fs::path(result_path, settings$reg_name, "aggr_harrell_c.rds"))
bma_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bma_harrell_c.rds"))

# Quick check ---------------------------------------------------------------------------------

bmr_tab = bmr_harrell_c$aggregate(measures = list(), conditions = TRUE)
bmr_tab[errors > 0,][, .(n = .N), by = .(task_id)]
bmr_tab[errors > 0,][, .(n = .N), by = .(learner_id)]

bma_harrell_c$friedman_posthoc(meas = measures_eval$harrell_c$id)

mlr3viz::autoplot(bma_harrell_c, type = "mean", meas = measures_eval$harrell_c$id)
mlr3viz::autoplot(bma_harrell_c, type = "box", meas = measures_eval$harrell_c$id)

mlr3viz::autoplot(bma_harrell_c, type = "fn", meas = measures_eval$harrell_c$id)

mlr3viz::autoplot(bma_harrell_c, type = "cd", meas = measures_eval$harrell_c$id, test = "nemenyi")
mlr3viz::autoplot(bma_harrell_c, type = "cd", meas = measures_eval$harrell_c$id, test = "bd", baseline = "CPH")


# Bulk-write all relevant plots ---------------------------------------------------------------

if (!fs::dir_exists(fs::path(result_path, settings$reg_name, "harrell_c"))) {
  fs::dir_create(fs::path(result_path, settings$reg_name, "harrell_c"))
}

for (measure_id in measures_eval_ids) {
  for (type in c("mean", "box", "fn", "cd")) {
    file_path = fs::path(result_path, settings$reg_name, "harrell_c", glue::glue("{measure_id}_{type}"))
    print(file_path)

    try({
      p = mlr3viz::autoplot(bma_harrell_c, type = type, meas = measure_id) +
        labs(
          x = "Learner",
          y = measures_eval[[measure_id]]$label,
          caption = glue::glue("Tuning measure: Harrell's C")
        ) +
        theme_minimal(base_size = 16)

      ggsave(fs::path_ext_set(file_path, "png"), plot = p, width = 9, height = 6, bg = "white")
      ggsave(fs::path_ext_set(file_path, "pdf"), plot = p, width = 9, height = 6, bg = "white")
    })

  }

}

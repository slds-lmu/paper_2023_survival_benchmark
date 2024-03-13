source(here::here("helpers.R"))
settings = config::get(config = "beartooth")

library(batchtools)
library(mlr3batchmark)
library(mlr3benchmark)
library(mlr3proba)
requireNamespace("mlr3extralearners")
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(ggplot2)

# reg_dir = here::here(settings$reg_name)
# reg = loadRegistry(reg_dir, writeable = TRUE)
# alljobs = collect_job_table(reg)
result_path = here::here("results")

# Store eval measures for easier retrieval
msr_tbl = measures_tbl()
measures_eval = get_measures_eval()
measures_eval_ids = msr_tbl$id

# Restoring from disk for further processing
bmr_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bmr_harrell_c.rds"))
bmrtab_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bmrtab_harrell_c.rds"))
bma_harrell_c  = readRDS(fs::path(result_path, settings$reg_name, "bma_harrell_c.rds"))

bmrtab_rcll  = readRDS(fs::path(result_path, settings$reg_name, "bmrtab_rcll.rds"))
bma_rcll  = readRDS(fs::path(result_path, settings$reg_name, "bma_rcll.rds"))

bma  = readRDS(fs::path(result_path, settings$reg_name, "bma_full.rds"))


# scores = bmr_harrell_c$score(measures_eval$harrell_c)
scores = bmr_harrell_c$score(msr("surv.rcll"))

# Quick check ---------------------------------------------------------------------------------
bmrtab_harrell_c[errors > 0,][, .(n = .N), by = .(task_id)]
bmrtab_harrell_c[errors > 0,][, .(n = .N), by = .(learner_id)]

bma_harrell_c$friedman_posthoc(meas = measures_eval$harrell_c$id)

autoplot(bma_harrell_c, type = "mean", meas = measures_eval$harrell_c$id)
autoplot(bma_harrell_c, type = "box", meas = measures_eval$harrell_c$id)

autoplot(bma_harrell_c, type = "fn", meas = measures_eval$harrell_c$id)

autoplot(bma_harrell_c, type = "cd", meas = measures_eval$harrell_c$id, test = "nemenyi", ratio = 1)
autoplot(bma_clean_harrell_c, type = "cd", meas = measures_eval$harrell_c$id, test = "nemenyi", ratio = 1)

autoplot(bma_harrell_c, type = "cd", meas = measures_eval$harrell_c$id, test = "bd", baseline = "CPH", ratio = 1)

plot_results(bma_clean_harrell_c, measure_id = "harrell_c", exclude_learners = c("KM", "NA"))

plot_results(bma_clean_harrell_c, measure_id = "logloss", exclude_learners = "")

# Bulk-write all relevant plots ---------------------------------------------------------------

if (!fs::dir_exists(fs::path(result_path, settings$reg_name, "harrell_c"))) {
  fs::dir_create(fs::path(result_path, settings$reg_name, "harrell_c"))
}

for (measure_id in measures_eval_ids) {
  for (type in c("mean", "box", "fn", "cd")) {

    if (msr_tbl[id == measure_id, erv] & type %in% c("fn", "cd")) next


    file_path = fs::path(result_path, settings$reg_name, "harrell_c", glue::glue("{measure_id}_{type}"))
    print(file_path)

    try({
      p = mlr3viz::autoplot(
        bma_harrell_c,
        type = type,
        meas = measure_id,
        minimize = msr_tbl[id == measure_id, minimize]
      ) +
        labs(
          x = NULL,
          y = measures_eval[[measure_id]]$label,
          caption = glue::glue("Tuning measure: Harrell's C")
        ) +
        theme_minimal(base_size = 16)

      ggsave(fs::path_ext_set(file_path, "png"), plot = p, width = 9, height = 6, bg = "white")
      ggsave(fs::path_ext_set(file_path, "pdf"), plot = p, width = 9, height = 6, bg = "white")
    })

  }
}


# Tables

bma


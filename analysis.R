root = here::here()
#source(file.path(root, "settings.R"))
#source(file.path(root, "settings_trial_mode.R"))

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
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(ggplot2)

reg_dir = file.path(root, settings$reg_name)
reg = loadRegistry(reg_dir, writeable = TRUE)

alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")
tasktab = read.csv(here::here("attic", "tasktab.csv"))
alljobs = ljoin(alljobs, tasktab, by = "task_id")
data.table::setkey(alljobs, job.id)

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


# Harrell's C ------------------------------------------------------------------------------------

tictoc::tic(msg = "collecting results: harrell_c")
bmr_harrell = reduceResultsBatchmark(findTagged("harrell_c"))
tictoc::toc()

tictoc::tic(msg = "mlr3benchmark aggregating results")
bma_harrell = mlr3benchmark::as_benchmark_aggr(bmr_harrell, meas = measures_eval)
tictoc::toc()


tictoc::tic(msg = "saving results")
saveRDS(bmr, "tmp/bmr_harrell.rds")
tictoc::toc()
tictoc::tic(msg = "collecting aggregated results")
aggr_harrell = bmr_harrell$aggregate(measures = measures_eval, conditions = TRUE)
tictoc::toc()
tictoc::tic(msg = "saving aggregated results")
saveRDS(aggr_harrell, "tmp/aggr_harrell.rds")
tictoc::toc()

bma_harrell$friedman_posthoc(meas = "harrell_c")


resamplings_with_error = aggr[errors > 0, nr]
bmr_harrell$resample_result(resamplings_with_error[1])$errors

mlr3viz::autoplot(bmr_harrell, measure = measures_eval$rcll)
mlr3viz::autoplot(bma_harrell, type = "cd", meas = "harrell_c")
mlr3viz::autoplot(bma_harrell, type = "box", meas = "harrell_c")


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

root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################
library("batchtools")
library("mlr3batchmark")
library(ggplot2)

reg = loadRegistry(reg_dir, writeable = TRUE)

alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

###################################################################################################
### Current State
###################################################################################################
print(getStatus())
print(unique(getErrorMessages()))

# ids = grepLogs(findErrors(), pattern = "incorrect number")
# summarizeExperiments(ids, by = c("learner_id"))
#
# ids = grepLogs(findErrors(), pattern = "Distribution")
# summarizeExperiments(ids, by = c("learner_id"))

ids = findExpired()
if (nrow(ids) > 0) {
  summarizeExperiments(ids, by = c("task_id"))
  summarizeExperiments(ids, by = c("learner_id"))
}

#showLog(ids[1])

#ids = ijoin(findExpired(), findExperiments(prob.pars = task_id == "child"))
#showLog(ids[1])

###################################################################################################
### Reduce results
###################################################################################################
# Store eval measures for easier retrieval
measures_eval = list(
  harrell_c = msr("surv.cindex", id = "harrell_c"),
  uno_c = msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  # Added as graf alternative for now as per RS
  rcll = msr("surv.rcll", id = "rcll"),

  graf_proper = msr("surv.graf", id = "graf", proper = TRUE),
  dcalib = msr("surv.dcalib", id = "dcalib"),

  intlogloss = msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  logloss = msr("surv.logloss", id = "logloss"),
  calib = msr("surv.calib_alpha", id = "calib")
)

tictoc::tic(msg = "collecting results")
bmr = reduceResultsBatchmark()
tictoc::toc()

tictoc::tic(msg = "saving results")
saveRDS(bmr, "tmp/bmr.rds")
tictoc::toc()
tictoc::tic(msg = "collecting aggregated results")
aggr = bmr$aggregate(measures = measures_eval, conditions = TRUE)
tictoc::toc()
tictoc::tic(msg = "saving aggregated results")
saveRDS(aggr, "tmp/aggr.rds")
tictoc::toc()


resamplings_with_error = aggr[errors > 0, nr]

mlr3viz::autoplot(bmr, measure = measures_eval$rcll)
# bmr$resample_result(resamplings_with_error[1])$errors

ggplot(aggr, aes(x = learner_id, y = harrell_c)) +
  facet_wrap(vars(task_id)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal(base_size = 14)

scores = bmr$score(measures_eval$rcll)

ggplot(scores, aes(x = learner_id, y = surv.rcll)) +
  facet_wrap(vars(task_id)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal()


# Fiddle ------------------------------------------------------------------
# Attempting to roughly group the learners for quick viz

lrn_tab = tibble::tribble(
  ~learner_id, ~mlr3id,            ~Name,             ~Type,
  "KM",        "surv.kaplan",      "Kaplan-Meier",    "Baseline",
  "NL",        "surv.nelson",      "Nelson-Aalen",    "Baseline",
  "AF",        "surv.akritas",     "Akritas",         "Baseline",
  "CPH",       "surv.coxph",       "Cox PH",          "Misc",
  "GLM",       "surv.cv_glmnet",   "glmnet",          "Misc",
  "Pen",       "surv.penalized",   "Penalized",       "Misc",
  "Par",       "surv.parametric",  "Parametric",      "Misc",
  "Flex",      "surv.flexible",    "Flexsurv Spline", "Misc",
  "SSVM",      "surv.svm",         "SVM",             "Misc",
  "RFSRC",     "surv.rfsrc",       "RFSRC",           "Forests",
  "RAN",       "surv.ranger",      "Ranger",          "Forests",
  "CIF",       "surv.cforest",     "CIF",             "Forests",
  "ORSF",      "surv.aorsf",       "ORSF",            "Forests",
  "RRT",       "surv.rpart",       "Rpart",           "Forests",
  "MBO",       "surv.mboost",      "MBoost",          "Boosting",
  "CoxB",      "surv.cv_coxboost", "CoxBoost",        "Boosting",
  "XGB",       "surv.xgboost",     "XGBoost",         "Boosting"
  # "CoxT",      "surv.coxtime",     "Cox-Time",        "Neural Network",
  # "DH",        "surv.deephit",     "DeepHit",         "Neural Network",
  # "DS",        "surv.deepsurv",    "DeepSurv",        "Neural Network",
  # "LH",        "surv.loghaz",      "Logistic-Hazard", "Neural Network",
  # "PCH",       "surv.pchazard",    "PC-Hazard",       "Neural Network",
  # "DNN",       "surv.dnnsurv",     "DNNSurv",         "Neural Network"
)
lrn_tab$Type = factor(lrn_tab$Type, levels = unique(lrn_tab$Type))

aggr = ljoin(aggr, lrn_tab)

aggr[, .(count = .N), by = task_id]

aggr |>
  #subset(task_id == "channing") |>
  ggplot(aes(x = learner_id, y = harrell_c)) +
  facet_grid(cols = vars(Type), rows = vars(task_id), scales = "free") +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal(base_size = 14)

aggr |>
  #subset(task_id == "channing") |>
  ggplot(aes(x = learner_id, y = harrell_c)) +
  coord_flip() +
  facet_grid(rows = vars(Type), cols = vars(task_id), scales = "free") +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal(base_size = 14)

aggr[harrell_c <= 0.5, ]

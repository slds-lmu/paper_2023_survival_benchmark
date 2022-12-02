root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################
library("batchtools")
library("mlr3batchmark")
library(ggplot2)

reg = loadRegistry("registry", writeable = TRUE)

###################################################################################################
### Current State
###################################################################################################
print(getStatus())
print(getErrorMessages())

ids = grepLogs(findErrors(), pattern = "incorrect number")
summarizeExperiments(ids, by = c("learner_id"))

ids = grepLogs(findErrors(), pattern = "Distribution")
summarizeExperiments(ids, by = c("learner_id"))

ids = findExpired()
summarizeExperiments(ids, by = c("task_id"))

ids = ijoin(findExpired(), findExperiments(prob.pars = task_id == "child"))
showLog(ids[1])

###################################################################################################
### Reduce results
###################################################################################################
# measures = msrs(c("surv.cindex", "surv.rcll"))
measures = list(
  # Tuning measures
  msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  msr("surv.rcll", id = "rcll"),
  msr("surv.dcalib", id = "dcalib"),

  # Additional evaluation measures
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.graf", id = "graf", proper = TRUE),
  msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  msr("surv.logloss", id = "logloss"),
  msr("surv.calib_alpha", id = "calib")
)

bmr = reduceResultsBatchmark()
saveRDS(bmr, "tmp/bmr.rds")

#profvis::profvis({
aggr = bmr$aggregate(measures = measures, conditions = TRUE)
#})
saveRDS(aggr, "tmp/aggr.rds")
resamplings_with_error = aggr[errors > 0, nr]

mlr3viz::autoplot(bmr)
# bmr$resample_result(resamplings_with_error[1])$errors

ggplot(aggr, aes(x = learner_id, y = surv.cindex)) +
  facet_wrap(vars(task_id)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal(base_size = 14)

scores = bmr$score(msrs(c("surv.rcll")))

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
  "RFSRC",     "surv.rfsrc",       "RFSRC",           "Trees",
  "RAN",       "surv.ranger",      "Ranger",          "Trees",
  "CIF",       "surv.cforest",     "CIF",             "Trees",
  "ORSF",      "surv.aorsf",       "ORSF",            "Trees",
  "RRT",       "surv.rpart",       "Rpart",           "Trees",
  "MBO",       "surv.mboost",      "MBoost",          "Boosting",
  "CoxB",      "surv.cv_coxboost", "CoxBoost",        "Boosting",
  "XGB",       "surv.xgboost",     "XGBoost",         "Boosting",
  "CoxT",      "surv.coxtime",     "Cox-Time",        "Neural Network",
  "DH",        "surv.deephit",     "DeepHit",         "Neural Network",
  "DS",        "surv.deepsurv",    "DeepSurv",        "Neural Network",
  "LH",        "surv.loghaz",      "Logistic-Hazard", "Neural Network",
  "PCH",       "surv.pchazard",    "PC-Hazard",       "Neural Network",
  "DNN",       "surv.dnnsurv",     "DNNSurv",         "Neural Network"
)
lrn_tab$Type = factor(lrn_tab$Type, levels = unique(lrn_tab$Type))

aggr = ljoin(aggr, lrn_tab)

aggr |>
  subset(task_id == "lung") |>
  ggplot(aes(x = learner_id, y = surv.cindex)) +
  facet_grid(cols = vars(Type), rows = vars(task_id), scales = "free") +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal(base_size = 14)

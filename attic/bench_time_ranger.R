root = here::here()
source(file.path(root, "settings_runtime_est.R"))

# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# See also attic/_dependencies.R
# renv::restore(prompt = FALSE)

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")


# Create Registry ---------------------------------------------------------
if (dir.exists(reg_dir)) {
  message("Loading registry with writable = TRUE")
  reg = loadRegistry(reg_dir, writeable = TRUE)
} else {
  message("Creating new registry")
  reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = seed,
                               packages = c("mlr3", "mlr3proba"))
}

# Create Tasks and corresponding instantiated Resamplings -----------------
set.seed(seed)
files = dir(file.path(root, "code", "data"), pattern = "\\.rds$", full.names = TRUE)
names = stringi::stri_sub(basename(files), 1, -5)
tasks = resamplings = mlr3misc::named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  # Just for runtime estimation: Do simple holdout
  if (all.equal(c(outer_folds, inner_folds), c(1, 1))) {
    folds = 1
    resampling = rsmp("holdout")$instantiate(task)
  } else {
    # normal CV
    folds = min(floor(task$nrow / min_obs), outer_folds)
    resampling = rsmp("cv", folds = folds)$instantiate(task)

    stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= min_obs))

    #save_resampling(resampling, names[i])
  }

  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, folds, resampling)
}


# Base learner setup ------------------------------------------------------
bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE,
                 method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  fallback$predict_type = "crank"
  learner$predict_type = "crank"

  learner$fallback = fallback
  learner$encapsulate = c(train = "callr", predict = "callr")

  # 1. fixfactors ensures factor levels are the same during train and predict
  # - might introduce missings, hence
  # 2. imputesample to impute them, analogously to robustify pipeline
  # 3. collapsefactors reduces the number of factor levels
  # notable cases: hdfail, bladder0, whas, aids.id with a) many b) rare factor levels
  # Proposed change LB:
  # - Switch to no_collapse_above_prevalence = 0.05 to collapse levels up to 5% prevalence
  # - Keep target_level_count = 5 to not reduce to fewer than 5 levels total
  # (see also attic/data_check_factors.Rmd)
  preproc = po("fixfactors") %>>%
    po("imputesample", affect_columns = selector_type("factor")) %>>%
    po("collapsefactors",
       no_collapse_above_prevalence = 0.05,
       target_level_count = 5)

  # scaling only used for SSVM if DL removed
  # Done before treatment encoding
  if (.scale) {
    preproc = preproc %>>%
      po("scale")
  }

  # treatment encoding only for selected learners that don't support handling factors
  # Note: encoding is done for glmnet but not for coxph. Both are internally a
  # cox model, but glmnet does not do the treatment encoding automatically
  if (.encode) {
    preproc = preproc %>>%
      po("encode", method = "treatment")
  }

  # removeconstants: should constant features be introduced, they're dropped.
  #  - Done after treatment encoding
  # Stack preprocessing on top of learner + distr stuff. 'form' as per RS
  preproc %>>%
    po("removeconstants") %>>%
    ppl("distrcompositor", learner = learner, form = "ph") |>
    # Need to convert to GraphLearner
    as_learner()
}


# Set tuning measures -----------------------------------------------------
# Tuning measures are a subset of all measures, remaining measures are used
# for evaluation (see overleaf Table 1)
measures = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.dcalib", id = "dcalib"),
  msr("surv.rcll", id = "rcll")
)


# Assemble learners -------------------------------------------------------

# survivalmodels::akritas
# https://raphaels1.github.io/survivalmodels/reference/akritas.html
AF = bl("surv.akritas", lambda = 0.5)
#surv.akritas.lambda = p_dbl(0, 1)

CPH = bl("surv.coxph")

RAN = bl("surv.ranger", num.trees = 500,
         splitrule = "logrank",
         min.node.size = 5,
         replace = TRUE)
# surv.ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
# surv.ranger.mtry.ratio = p_dbl(0, 1),
# surv.ranger.min.node.size = p_int(1, 50),
# surv.ranger.replace = p_lgl(),
# surv.ranger.sample.fraction = p_dbl(0, 1)
CoxB = bl("surv.cv_coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 500, .encode = TRUE)
XGB = bl("surv.xgboost", tree_method = "hist", booster = "gbtree", .encode = TRUE)
# surv.xgboost.max_depth = p_int(1, 20),
# surv.xgboost.subsample = p_dbl(0, 1),
# surv.xgboost.colsample_bytree = p_dbl(0, 1),
# surv.xgboost.nrounds = p_int(10, 5000),
# surv.xgboost.eta = p_dbl(0, 1),
# surv.xgboost.grow_policy = p_fct(c("depthwise", "lossguide"))

learners = list(akritas = AF, ranger = RAN, CoxBoost = CoxB, XGB = XGB)
imap(learners, function(l, id) l$id = id)

grid = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = rsmp("holdout")
)

future::plan("multisession", workers = 40)
lgr::get_logger("mlr3")$set_threshold("debug")

bmr = benchmark(
  grid, store_models = TRUE, encapsulate = "callr"
)


scores = bmr$score(msrs(c("time_train", "time_predict")))
aggr = bmr$aggregate(conditions = TRUE)

if (!fs::dir_exists("tmp-ranger-timing")) fs::dir_create("tmp-ranger-timing")
saveRDS(bmr, here::here("tmp-ranger-timing", "bmr.rds"))

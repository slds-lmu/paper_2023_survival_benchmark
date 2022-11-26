root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages ----
###################################################################################################

# Not on CRAN anymore
remotes::install_github("mlr-org/mlr3proba")
remotes::install_github("RaphaelS1/survivalmodels")

# Not on CRAN
remotes::install_github("mlr-org/mlr3batchmark")
remotes::install_github("mlr-org/mlr3extralearners")

# Packages for specific learners
remotes::install_github("binderh/CoxBoost") # Not on CRAN anymore

library("stringi")
library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")


# Create Registry ---------------------------------------------------------
if (dir.exists(reg_dir)) {
  reg = loadRegistry(reg_dir, writeable = TRUE)
} else {
  reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = seed,
    packages = c("mlr3", "mlr3proba"))
}

# Create Tasks and corresponding instantiated Resamplings -----------------
set.seed(seed)
files = dir(file.path(root, "code", "data"), pattern = "\\.rds$", full.names = TRUE)
names = stri_sub(basename(files), 1, -5)
tasks = resamplings = named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])

  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  task$set_col_roles("status", add_to = "stratum")

  folds = min(floor(task$nrow / min_obs), outer_folds)
  resampling = rsmp("cv", folds = folds)$instantiate(task)
  stopifnot(all(as.data.table(resampling)[set == "test"][, .N, by = "iteration"]$N >= min_obs))

  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, folds, resampling)
}


# Create Learners and populate Registry -----------------------------------
bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)
  
  # As per RS to fix #38
  fallback$predict_type = "crank"
  learner$predict_type = "crank"
  
  learner$fallback = fallback
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  
  # Added form as per RS
  g = ppl("distrcompositor", learner = learner, form = 'ph')

  if (.scale) {
    g = po("scale") %>>% g
  }

  if (.encode) {
    g = po("encode", method = "treatment") %>>% g
  }

  as_learner(po("fixfactors") %>>% po("collapsefactors", target_level_count = 5) %>>% g)
}

auto_tune = function(learner, ...) { # wrap into random search
  learner = as_learner(learner)
  search_space = ps(...)
  if (is.null(search_space$trafo))
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))

  at = AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("cv", folds = inner_folds),
    measure = measure,
    terminator = trm("evals", n_evals = budget_constant, k = budget_multiplier),
    tuner = tnr("random_search"),
    store_tuning_instance = TRUE,
    store_benchmark_result = FALSE,
    store_models = FALSE
  )
}

# Tuning measures are a subset of all measures, remaining measures are used
# for evaluation (see overleaf Table 1)
measures = list(
  # msr("surv.cindex", id = "harrell_c"),
  msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  # Added as graf alternative for now as per RS
  msr("surv.rcll", id = "rcll"),
  #msr("surv.graf", id = "graf", proper = TRUE),
  msr("surv.dcalib", id = "dcalib")
  
  #msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  #msr("surv.logloss", id = "logloss"),
  #msr("surv.calib_alpha", id = "calib")
)

for (measure in measures) {
  learners = list(
    KM = bl("surv.kaplan")

    ,

    NL = bl("surv.nelson")

    ,

    AF = auto_tune(
      bl("surv.akritas"),
      surv.akritas.lambda = p_dbl(0, 1)
    )

    ,

    CPH = bl("surv.coxph")

    ,


    GLM = auto_tune(
      bl("surv.cv_glmnet", .encode = TRUE),
      surv.cv_glmnet.alpha = p_dbl(0, 1)
    )

    ,

    Pen = auto_tune(
      bl("surv.penalized"),
      surv.penalized.lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
      surv.penalized.lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
    )

    ,

    Par = auto_tune(
      bl("surv.parametric", type = "aft"),
      surv.parametric.dist = p_fct(c("weibull", "lognormal", "loglogistic"))
    )

    ,

    Flex = auto_tune(
      bl("surv.flexible"),
      surv.flexible.k = p_int(1, 10)
    )

    ,

    RFSRC = auto_tune(
      bl("surv.rfsrc", ntree = 5000),
      surv.rfsrc.splitrule = p_fct(c("bs.gradient", "logrank")),
      surv.rfsrc.mtry.ratio = p_dbl(0, 1),
      surv.rfsrc.nodesize = p_int(1, 50),
      surv.rfsrc.samptype = p_fct(c("swr", "swor")),
      surv.rfsrc.sampsize.ratio = p_dbl(0, 1)
    )

    ,

    RAN = auto_tune(
      bl("surv.ranger", num.trees = 5000),
      surv.ranger.splitrule = p_fct(c("C", "maxstat", "logrank")),
      surv.ranger.mtry.ratio = p_dbl(0, 1),
      surv.ranger.min.node.size = p_int(1, 50),
      surv.ranger.replace = p_lgl(),
      surv.ranger.sample.fraction = p_dbl(0, 1)
    )

    ,

    CIF = auto_tune(
      bl("surv.cforest", ntree = 5000),
      surv.cforest.mtryratio = p_dbl(0, 1),
      surv.cforest.minsplit = p_int(1, 50),
      surv.cforest.mincriterion = p_dbl(0, 1),
      surv.cforest.replace = p_lgl(),
      surv.cforest.fraction = p_dbl(0, 1)
    )

    ,

    ORSF = auto_tune(
      bl("surv.aorsf", ntree = 5000),
      surv.aorsf.mtry_ratio = p_dbl(0, 1),
      surv.aorsf.leaf_min_events = p_int(5, 50),
      control_type = "net",
      surv.aorsf.control_net_alpha = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.aorsf.split_min_obs = x$surv.aorsf.leaf_min_events + 5L
        x
      }
    )

    ,

    RRT = auto_tune(
      bl("surv.rpart"),
      surv.rpart.minbucket = p_int(5, 50)
    )

    ,

    MBO = auto_tune(bl("surv.mboost"),
      surv.mboost.family = p_fct(c("gehan", "cindex", "coxph", "weibull")),
      surv.mboost.mstop = p_int(10, 5000),
      surv.mboost.nu = p_dbl(0, 0.1),
      surv.mboost.baselearner = p_fct(c("bols", "btree"))
    )

    ,

    CoxB = bl("surv.cv_coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 5000, .encode = TRUE)

    ,

    XGB = auto_tune(
      bl("surv.xgboost", tree_method = "hist", booster = "gbtree", .encode = TRUE),
      surv.xgboost.max_depth = p_int(1, 20),
      surv.xgboost.subsample = p_dbl(0, 1),
      surv.xgboost.colsample_bytree = p_dbl(0, 1),
      surv.xgboost.nrounds = p_int(10, 5000),
      surv.xgboost.eta = p_dbl(0, 1),
      surv.xgboost.grow_policy = p_fct(c("depthwise", "lossguide"))
    )

    ,

    SSVM = auto_tune(
      bl("surv.svm", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3", .encode = TRUE, .scale = TRUE),
      surv.svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
      surv.svm.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x),
      .extra_trafo = function(x, param_set) {
        x$surv.svm.gamma.mu = c(x$surv.svm.gamma, x$surv.svm.mu)
        x$surv.svm.gamma = x$surv.svm.mu = NULL
        x
      }
    )

    ,

    CoxT = auto_tune(
      bl("surv.coxtime", standardize_time = TRUE, epochs = 100, optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.coxtime.dropout = p_dbl(0, 1),
      surv.coxtime.weight_decay = p_dbl(0, 0.5),
      surv.coxtime.learning_rate = p_dbl(0, 1),
      surv.coxtime.nodes = p_int(1, 32),
      surv.coxtime.k = p_int(1, 4),
      .extra_trafo = function(x, param_set) {
        x$surv.coxtime.num_nodes = rep(x$surv.coxtime.nodes, x$surv.coxtime.k)
        x$surv.coxtime.nodes = x$surv.coxtime.k = NULL
        x
      }
    )

    ,

    DH = auto_tune(
      bl("surv.deephit", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.deephit.nodes = p_int(1, 32),
      surv.deephit.k = p_int(1, 4),
      surv.deephit.dropout = p_dbl(0, 1),
      surv.deephit.weight_decay = p_dbl(0, 5),
      surv.deephit.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.deephit.num_nodes = rep(x$surv.deephit.nodes, x$surv.deephit.k)
        x$surv.deephit.nodes = x$surv.deephit.k = NULL
        x
      }
    )

    ,

    DS = auto_tune(
      bl("surv.deepsurv", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.deepsurv.nodes = p_int(1, 32),
      surv.deepsurv.k = p_int(1, 4),
      surv.deepsurv.dropout = p_dbl(0, 1),
      surv.deepsurv.weight_decay = p_dbl(0, 5),
      surv.deepsurv.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.deepsurv.num_nodes = rep(x$surv.deepsurv.nodes, x$surv.deepsurv.k)
        x$surv.deepsurv.nodes = x$surv.deepsurv.k = NULL
        x
      }
    )

    ,

    LH = auto_tune(
      bl("surv.loghaz", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.loghaz.nodes = p_int(1, 32),
      surv.loghaz.k = p_int(1, 4),
      surv.loghaz.dropout = p_dbl(0, 1),
      surv.loghaz.weight_decay = p_dbl(0, 5),
      surv.loghaz.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.loghaz.num_nodes = rep(x$surv.loghaz.nodes, x$surv.loghaz.k)
        x$surv.loghaz.nodes = x$surv.loghaz.k = NULL
        x
      }
    )

    ,

    PCH = auto_tune(
      bl("surv.pchazard", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.pchazard.nodes = p_int(1, 32),
      surv.pchazard.k = p_int(1, 4),
      surv.pchazard.dropout = p_dbl(0, 1),
      surv.pchazard.weight_decay = p_dbl(0, 5),
      surv.pchazard.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.pchazard.num_nodes = rep(x$surv.pchazard.nodes, x$surv.pchazard.k)
        x$surv.pchazard.nodes = x$surv.pchazard.k = NULL
        x
      }
    )

    ,

    DNN = auto_tune(
      bl("surv.dnnsurv", optimizer = "adam", epochs = 100, .encode = TRUE, .scale = TRUE),
      surv.dnnsurv.decay = p_dbl(0, 0.5),
      surv.dnnsurv.lr = p_dbl(0, 1),
      surv.dnnsurv.cuts = p_int(3, 50)
    )
  )

  imap(learners, function(l, id) l$id = id)

  # custom grid design (with instantiated resamplings)
  grid = cross_join(list(task = tasks, learner = learners), sorted = FALSE)
  grid$resampling = rep(resamplings, each = length(learners))
  ids = batchmark(grid, store_models = FALSE)
  addJobTags(ids, measure$id)

  # also tag jobs which have been skipped because they are not wrapped
  # into a AutoTuner (and as such don't differ)
  learners_skipped = ids(learners)[!map_lgl(learners, inherits, "AutoTuner")]
  ids = findExperiments(algo.pars = learner_id %in% learners_skipped)
  addJobTags(ids, measure$id)
}


summarizeExperiments(by = c("task_id", "learner_id"))


###################################################################################################
### Pretest
###################################################################################################
res = list(walltime = 4 * 3600, memory = 4096)
ids = findExperiments(repls = 1)
ids = ijoin(ids, findTagged("rcll"))

# submitJobs(ids, resources = res)

###################################################################################################
### Submit
###################################################################################################

if (FALSE) {
  tab = unnest(getJobTable(), c("prob.pars", "algo.pars"))
  unique(tab$task_id)
  unique(tab$learner_id)

  # this would be a good first start on the cluster
  ids = findExperiments(repls = 1)
  # Was 'rats' task but that dataset has been excluded since
  ids = ijoin(ids, findExperiments(prob.pars = task_id == "lung"))
  submitJobs(ids)

  summarizeExperiments(findErrors(), by = "learner_id")
  getErrorMessages()
}

if (FALSE) {
  ids = ajoin(ids, findDone())
  submitJobs(ids, resources = resources)

  getJobStatus()
  findErrors()

  getErrorMessages(findErrors())

}

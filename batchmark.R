root = here::here()
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################

# requires devel version of bbotk and mlr3tuning
devtools::install_github("mlr-org/bbotk")
devtools::install_github("mlr-org/mlr3tuning")
devtools::install_github("mlr-org/mlr3batchmark")
#devtools::install_github("mlr-org/mlr3extralearners")
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

###################################################################################################
### Create Registry
###################################################################################################
reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = seed,
  packages = c("mlr3", "mlr3proba"))
reg$cluster.functions = makeClusterFunctionsMulticore(4)


###################################################################################################
### Create Tasks and corresponding instantiated Resamplings
###################################################################################################
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



###################################################################################################
### Create Learners and populate Registry
###################################################################################################
bl = function(key, ...) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)
  learner$fallback = fallback
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner
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


for (measure in list(msr("surv.cindex"), msr("surv.graf", proper = TRUE))) {
  learners = list(
    KM = bl("surv.kaplan"),

    NL = bl("surv.nelson"),

    AF = auto_tune(
      bl("surv.akritas"),
      lambda = p_dbl(0, 1)
    ),

    CPH = bl("surv.coxph"),


    GLM = auto_tune(
      po("encode", method = "treatment") %>>% bl("surv.cv_glmnet"),
      surv.cv_glmnet.alpha = p_dbl(0, 1)
    ),

    Pen = auto_tune(
      bl("surv.penalized"),
      lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
      lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
    ),

    Par = auto_tune(
      bl("surv.parametric", type = "aft"),
      dist = p_fct(c("weibull", "lognormal", "loglogistic"))
    ),

    Flex = auto_tune(
      bl("surv.flexible"),
      k = p_int(1, 10)
    ),

    RFSRC = auto_tune(
      bl("surv.rfsrc", ntree = 5000),
      splitrule = p_fct(c("bs.gradient", "logrank")),
      mtry.ratio = p_dbl(0, 1),
      nodesize = p_int(1, 50),
      samptype = p_fct(c("swr", "swor")),
      sampsize.ratio = p_dbl(0, 1)
    ),


    RAN = auto_tune(
      bl("surv.ranger", num.trees = 5000),
      splitrule = p_fct(c("C", "maxstat", "logrank")),
      mtry.ratio = p_dbl(0, 1),
      min.node.size = p_int(1, 50),
      replace = p_lgl(),
      sample.fraction = p_dbl(0, 1)
    ),


    CIF = auto_tune(
      bl("surv.cforest", ntree = 5000),
      mtryratio = p_dbl(0, 1),
      minsplit = p_int(1, 50),
      mincriterion = p_dbl(0, 1),
      replace = p_lgl(),
      fraction = p_dbl(0, 1)
    ),


    ORSF = auto_tune(
      bl("surv.obliqueRSF", ntree = 5000),
      mtry_ratio = p_dbl(0, 1),
      use.cv = p_lgl(),
      min_events_in_leaf_node = p_int(5, 50),
      alpha = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$min_obs_to_split_node = x$min_events_in_leaf_node + 5L
        x
      }
    ),

    RRT = auto_tune(
      bl("surv.rpart"),
      minbucket = p_int(5, 50)
    ),

    MBO = auto_tune(bl("surv.mboost"),
      family = p_fct(c("gehan", "cindex", "coxph", "weibull")),
      mstop = p_int(10, 5000),
      nu = p_dbl(0, 0.1),
      baselearner = p_fct(c("bols", "btree"))
    ),

    CoxB = bl("surv.cv_coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 5000),

    XGB = auto_tune(
      # FIXME: tree_method and booster missing in overleaf
      po("encode", method = "treatment") %>>% bl("surv.xgboost", tree_method = "hist", booster = "gbtree"),
      surv.xgboost.max_depth = p_int(1, 20),
      surv.xgboost.subsample = p_dbl(0, 1),
      surv.xgboost.colsample_bytree = p_dbl(0, 1),
      surv.xgboost.nrounds = p_int(10, 5000),
      surv.xgboost.eta = p_dbl(0, 1),
      surv.xgboost.grow_policy = p_fct(c("depthwise", "lossguide"))
    ),

    SSVM = auto_tune(
      po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.svm", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3"),
      surv.svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
      surv.svm.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
      surv.svm.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x),
      .extra_trafo = function(x, param_set) {
        x$surv.svm.gamma.mu = c(x$surv.svm.gamma, x$surv.svm.mu)
        x$surv.svm.gamma = x$surv.svm.mu = NULL
        x
      }
    ),

    CoxT = auto_tune(
      po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.coxtime", standardize_time = TRUE, epochs = 100, optimizer = "adam"),
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
    ),

    DH = auto_tune(
      bl("surv.deephit", optimizer = "adam"),
      nodes = p_int(1, 32),
      k = p_int(1, 4),
      dropout = p_dbl(0, 1),
      weight_decay = p_dbl(0, 5),
      learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$num_nodes = rep(x$nodes, x$k)
        x$nodes = x$k = NULL
        x
      }
    ),

    DS = auto_tune(
      bl("surv.deepsurv", optimizer = "adam"),
      nodes = p_int(1, 32),
      k = p_int(1, 4),
      dropout = p_dbl(0, 1),
      weight_decay = p_dbl(0, 5),
      learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$num_nodes = rep(x$nodes, x$k)
        x$nodes = x$k = NULL
        x
      }
    ),

    LH = auto_tune(
      bl("surv.loghaz", optimizer = "adam"),
      nodes = p_int(1, 32),
      k = p_int(1, 4),
      dropout = p_dbl(0, 1),
      weight_decay = p_dbl(0, 5),
      learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$num_nodes = rep(x$nodes, x$k)
        x$nodes = x$k = NULL
        x
      }
    ),

    PCH = auto_tune(
      bl("surv.pchazard", optimizer = "adam"),
      nodes = p_int(1, 32),
      k = p_int(1, 4),
      dropout = p_dbl(0, 1),
      weight_decay = p_dbl(0, 5),
      learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$num_nodes = rep(x$nodes, x$k)
        x$nodes = x$k = NULL
        x
      }
    ),

    DNN = auto_tune(
      bl("surv.dnnsurv", optimizer = "adam", epochs = 100),
      decay = p_dbl(0, 0.5),
      lr = p_dbl(0, 1),
      cuts = p_int(3, 50)
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
findTagged("surv.graf")
findTagged("surv.harrell_c")


###################################################################################################
### Submit
###################################################################################################

if (FALSE) {
  tab = unnest(getJobTable(), c("prob.pars", "algo.pars"))
  unique(tab$task_id)
  unique(tab$learner_id)

  # this would be a good first start on the cluster
  ids = findExperiments(repls = 1)
  ids = ijoin(ids, findExperiments(prob.pars = task_id == "rats"))
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

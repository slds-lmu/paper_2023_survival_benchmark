### Todo:
# * tune for 3 measures (graf, c-index, ...?)
# * budget: 50 * p
# * discuss what to store

root = here::here()
seed = 123
source(file.path(root, "settings.R"))

###################################################################################################
### Packages
###################################################################################################

# requires devel version of bbotk
devtools::install_github("mlr-org/bbotk")
library("stringi")
library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")

if (length(find.package("mlr3batchmark", quiet = TRUE)) == 0L) {
  devtools::install_github("mlr-org/mlr3batchmark")
}
library("mlr3batchmark")

if (length(find.package("mlr3extralearners", quiet = TRUE)) == 0L) {
  devtools::install_github("mlr-org/mlr3extralearners")
}
requireNamespace("mlr3extralearners")



###################################################################################################
### Create Tasks corresponding instantiated Resamplings
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
### Create Learners
###################################################################################################
bl = function(key, id, ...) { # get base learner with fallback + encapsulation
  learner = lrn(key, id = id, ...)
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

  AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("cv", folds = inner_folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = budget_constant, k = budget_multiplier),
    tuner = tnr("random_search")
  )
}

learners = list(
  bl("surv.kaplan", id = "KM"),

  bl("surv.nelson", id = "Nel"),

  auto_tune(
    bl("surv.akritas", id = "AE"),
    lambda = p_dbl(0, 1)
  ),

  bl("surv.coxph", id = "CPH"),


  auto_tune(
    po("encode", method = "treatment") %>>% bl("surv.cv_glmnet", id = "GLM"),
    GLM.alpha = p_dbl(0, 1)
  ),

  auto_tune(
    bl("surv.penalized", id = "Pen"),
    lambda1 = p_dbl(-10, 10, trafo = function(x) 2^x),
    lambda2 = p_dbl(-10, 10, trafo = function(x) 2^x)
  ),

  auto_tune(
    bl("surv.parametric", id = "Par", type = "aft"),
    dist = p_fct(c("weibull", "lognormal", "loglogistic"))
  ),

  auto_tune(
    bl("surv.flexible", id = "Flex"),
    k = p_int(1, 10)
  ),

  auto_tune(
    bl("surv.rfsrc", id = "RFSRC", ntree = 5000),
    splitrule = p_fct(c("bs.gradient", "logrank")),
    mtry.ratio = p_dbl(0, 1),
    nodesize = p_int(1, 50),
    samptype = p_fct(c("swr", "swor")),
    sampsize.ratio = p_dbl(0, 1)
  ),


  auto_tune(
    bl("surv.ranger", id = "RAN", num.trees = 5000),
    splitrule = p_fct(c("C", "maxstat", "logrank")),
    mtry.ratio = p_dbl(0, 1),
    min.node.size = p_int(1, 50),
    replace = p_lgl(),
    sample.fraction = p_dbl(0, 1)
  ),


  auto_tune(
    bl("surv.cforest", id = "CIF", ntree = 5000),
    mtryratio = p_dbl(0, 1),
    minsplit = p_int(1, 50),
    mincriterion = p_dbl(0, 1),
    replace = p_lgl(),
    fraction = p_dbl(0, 1)
  ),


  auto_tune(
    bl("surv.obliqueRSF", id = "ORSF", ntree = 5000),
    mtry_ratio = p_dbl(0, 1),
    use.cv = p_lgl(),
    min_events_in_leaf_node = p_int(5, 50),
    alpha = p_dbl(0, 1),
    .extra_trafo = function(x, param_set) {
      x$min_obs_to_split_node = x$min_events_to_split_node + 5L
      x
    }
  ),

  auto_tune(
    bl("surv.rpart", id = "RRT"),
    minbucket = p_int(5, 50)
  ),

  auto_tune(bl("surv.mboost", id = "MBO"),
    family = p_fct(c("gehan", "cindex", "coxph", "weibull")),
    mstop = p_int(10, 5000),
    nu = p_dbl(0, 0.1),
    baselearner = p_fct(c("bols", "btree"))
  ),

  bl("surv.cv_coxboost", id = "CoxB", penalty = "optimCoxBoostPenalty", maxstepno = 5000),

  auto_tune(
    po("encode", method = "treatment") %>>% bl("surv.xgboost", id = "XGB"),
    XGB.max_depth = p_int(1, 20),
    XGB.subsample = p_dbl(0, 1),
    XGB.colsample_bytree = p_dbl(0, 1),
    XGB.nrounds = p_int(10, 5000),
    XGB.eta = p_dbl(-5, 5, trafo = function(x) 2^x),
    XGB.grow_policy = p_fct(c("depthwise", "lossguide"))
  ),

  auto_tune(
    po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.svm", id = "SSVM", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3"),
    SSVM.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")),
    SSVM.gamma = p_dbl(-10, 10, trafo = function(x) 10^x),
    SSVM.mu = p_dbl(-10, 10, trafo = function(x) 10^x),
    SSVM.kernel.pars = p_dbl(-5, 5, trafo = function(x) 2^x),
    .extra_trafo = function(x, param_set) {
      x$SSVM.gamma.mu = c(x$SSVM.gamma, x$SSVM.mu)
      x$SSVM.gamma = x$SSVM.mu = NULL
      x
    }
  ),

  auto_tune(
    po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.coxtime", id = "CoxT", standardize_time = TRUE, epochs = 100),
    CoxT.dropout = p_dbl(0, 1),
    CoxT.weight_decay = p_dbl(0, 0.5),
    CoxT.learning_rate = p_dbl(0, 1),
    CoxT.num_nodes_i = p_int(1, 32),
    CoxT.num_nodes_k = p_int(1, 4),
    .extra_trafo = function(x, param_set) {
      x$CoxT.num_nodes = x$CoxT.num_nodes_i^x$CoxT.num_nodes_k
      x$CoxT.num_nodes_i = x$CoxT.num_nodes_k = NULL
      x
    }
  )
)

###################################################################################################
### Create Registry + populate with experiments
###################################################################################################
reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = seed,
  packages = c("mlr3", "mlr3proba"))

# custom grid design (with instantiated resamplings)
grid = cross_join(list(task = tasks, learner = learners), sorted = FALSE)
grid$resampling = rep(resamplings, each = length(learners))

batchmark(grid, store_models = FALSE)

summarizeExperiments(by = c("task_id", "learner_id"))


###################################################################################################
### Submit
###################################################################################################

if (FALSE) {
  tab = unnest(getJobTable(), c("prob.pars", "algo.pars"))
  unique(tab$task_id)
  unique(tab$learner_id)

  # some small id sets to toy around
  ids = findExperiments(
    prob.pars = task_id %in% c("lung", "whas"),
    algo.pars = learner_id %in% c("class_semipar_coxph", "class_nonpar_kaplan", "ml_ranfor_ranger_c.tuned"),
    repls = 1:3
  )

  # this would be a good first start on the cluster
  ids = findExperiments(repls = 1)
}

if (FALSE) {
  ids = ajoin(ids, findDone())
  submitJobs(ids, resources = resources)
}

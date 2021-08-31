### Todo:
# * tune for 3 measures (graf, c-index, ...?)
# * budget: 50 * p
# * discuss what to store

root = here::here()
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
  bl("surv.kaplan", id = "kaplan"),

  auto_tune(
    bl("surv.akritas", id = "akritas"), 
    lambda = p_dbl(0, 1)
  ),

  bl("surv.coxph", id = "coxph"),


  auto_tune(
    po("encode", method = "treatment") %>>% bl("surv.cv_glmnet", id = "cvglmnet"), 
    cvglmnet.alpha = p_dbl(0, 1)
  ),

  auto_tune(
    bl("surv.penalized", id = "penalized"), 
    lambda1 = p_dbl(0, 10), 
    lambda2 = p_dbl(0, 10)
  ),

  auto_tune(
    bl("surv.parametric", id = "parametric", type = "aft"), 
    dist = p_fct(c("weibull", "logistic", "lognormal", "loglogistic"))
  ),

  auto_tune(
    bl("surv.flexible", id = "flexible"), 
    k = p_int(1, 10)
  ),

  auto_tune(
    bl("surv.rfsrc", id = "rfsrc", ntree = 5000),
    splitrule = p_fct(c("bs.gradient", "logrank")),
    mtry = p_int(1, 12),  # FIXME mtry patch
    nodesize = p_int(1, 50),
    samptype = p_fct(c("swr", "swor")),
    sampsize = p_int(1, 2) #  FIXME patch
  ),


  auto_tune(
    bl("surv.ranger", id = "ranger", num.trees = 5000),
    splitrule = p_fct(c("C", "maxstat", "logrank")),
    mtry.ratio = p_dbl(0.3, 1), # FIXME lower bound?
    min.node.size = p_int(1, 50),
    replace = p_lgl(),
    sample.fraction = p_dbl(0, 1)
  ),


  auto_tune(
    bl("surv.cforest", id = "cforest", ntree = 5000),
    mtry = p_int(1, 12), # FIXME patch
    minsplit = p_int(1, 50),
    mincriterion = p_dbl(0, 1),
    replace = p_lgl(),
    fraction = p_dbl(0, 1)
  ),


  auto_tune(
    bl("surv.obliqueRSF", id = "obliqueRSF", ntree = 5000),
    alpha = p_dbl(0, 1),
    use_cv = p_lgl(),
    min_obs_to_split_node = p_int(1, 50),
    mtry = p_int(1, 12) # FXIME patch
  ),

  auto_tune(
    bl("surv.rpart", id = "rpart"),
    minbucket = p_int(1, 50), 
    cp = p_dbl(-10, -1, trafo = function(x) 10^x)
  ),

  auto_tune(bl("surv.mboost", id = "mboost"),
    familiy = p_fct(c("gehan", "cindex", "coxph")),
    mstop = p_int(10, 5000), 
    nu = p_dbl(0, 0.1), 
    baselearner = p_fct(c("bols", "btree"))
  ),

  bl("surv.cv_coxboost", id = "coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 5000),

  auto_tune(
    po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.svm", id = "svm", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3"),
    svm.kernel = p_fct(c("lin_kernel", "rbf_kernel", "add_kernel")), 
    svm.gamma = p_dbl(-3, 3, trafo = function(x) 10^x),
    svm.mu = p_dbl(-3, 3, trafo = function(x) 10^x)
  ),


  auto_tune(
    bl("surv.xgboost", id = "xgboost"),
    maxdepth = p_int(1, 20),
    subsample = p_dbl(0, 1),
    colsample_bytree = p_dbl(0, 1),
    nrounds = p_int(10, 5000), 
    eta = p_dbl(0, 0.1)
  )


  # FIXME neural networks
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

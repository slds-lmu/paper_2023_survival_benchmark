### Todo:
# * resampling: ensure 30 obs per fold 
# * resampling: ensure 1 uncens obs per fold
# * tune for 3 measures (graf, c-index, ...?)
# * budget: 50 * p
# * discuss what to store

library(mlr3)
library(mlr3misc)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3tuning)
library(batchtools)
library(mlr3batchmark)

###################################################################################################
### Tasks
###################################################################################################
files = dir(file.path(here::here(), "code", "data"), pattern = "\\.rds$", full.names = TRUE)
tasks = lapply(files, function(f) {
  id = basename(f)
  id = substr(id, 1, nchar(id)-4)
  data = readRDS(f)
  as_task_surv(data, target = "time", event = "status", id = id)
})
names(tasks) = ids(tasks)

###################################################################################################
### Learners
###################################################################################################
bl = function(key, id, ...) { # get base learner with fallback + encapsulation
  learner = lrn(key, id = id, ...)
  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)
  learner$fallback = fallback
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner
}

auto_tune = function(learner, search_space, folds = 3L, n_evals = 100L) { # wrap into random search
  # FIXME: do we want to stick to a fixed budget?
  #        should it depend on the dimension of search_space?
  AutoTuner$new(
    learner = as_learner(learner),
    search_space = search_space,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search")
  )
}

learners = list(
  bl("surv.kaplan", id = "class_nonpar_kaplan"),

  auto_tune(bl("surv.akritas", id = "class_nonpar_akritas"), ps(lambda = p_dbl(0, 1))),

  bl("surv.coxph", id = "class_semipar_coxph"),

  auto_tune(po("encode", method = "treatment") %>>% bl("surv.cv_glmnet", id = "class_semipar_cvglmnet"), ps(alpha = p_dbl(0, 1))),

  auto_tune(bl("surv.penalized", id = "class_semipar_penalized"), ps(lambda1 = p_dbl(0, 10), lambda2 = p_dbl(0, 10))),

  auto_tune(bl("surv.parametric", id = "class_par_param", type = "aft"), ps(dist = p_fct(c("weibull", "logistic", "lognormal", "loglogistic")))),

  auto_tune(bl("surv.flexible", id = "class_par_flex"), ps(k = p_int(1, 7))),

  auto_tune(bl("surv.rfsrc", id = "ml_ranfor_rfsrc_brier", splitrule = "bs.gradient"),
    ps(ntree = p_int(250, 5000), mtry = p_int(1, 12), nodesize = p_int(1, 20))),

  # FIXME: Why is splitrule not regular hyperparameter? Where do we draw the line?
  auto_tune(bl("surv.rfsrc", id = "ml_ranfor_rfsrc_logrank", splitrule = "logrank"),
    ps(ntree = p_int(250, 5000), mtry = p_int(1, 12), nodesize = p_int(1, 20))),

  auto_tune(bl("surv.ranger", id = "ml_ranfor_ranger_c", splitrule = "C"),
    ps(num.trees = p_int(250, 5000), mtry = p_int(1, 12), min.node.size = p_int(1, 20))),

  # FIXME: Copy-paste error here
  auto_tune(bl("surv.ranger", id = "ml_ranfor_ranger_c", splitrule = "C"),
    ps(num.trees = p_int(250, 5000), mtry = p_int(1, 12), min.node.size = p_int(1, 20))),

  auto_tune(bl("surv.cforest", id = "ml_ranfor_rscif"),
    ps(ntree = p_int(250, 5000), mtry = p_int(1, 12))),

  auto_tune(bl("surv.rpart", id = "ml_ranfor_rrt"),
    ps(minbucket = p_int(1, 20), maxdepth = p_int(2, 30))),

  auto_tune(bl("surv.xgboost", id = "ml_gbm_xgboost"),
    ps(nrounds = p_int(10, 2500), eta = p_dbl(0, 0.01), gbtree = p_fct(c("gbtree", "gblinear", "dart")), maxdepth = p_int(1, 10))),

  auto_tune(bl("surv.mboost", id = "ml_gbm_mboost_coxph", family = "coxph"),
    ps(mstop = p_int(10, 2500), nu = p_dbl(0, 0.01), baselearner = p_fct(c("bols", "btree")))),

  auto_tune(bl("surv.mboost", id = "ml_gbm_mboost_cindex", family = "cindex"),
    ps(mstop = p_int(10, 2500), nu = p_dbl(0, 0.01), baselearner = p_fct(c("bols", "btree")))),

  auto_tune(bl("surv.mboost", id = "ml_gbm_mboost_gehan", family = "gehan"),
    ps(mstop = p_int(10, 2500), nu = p_dbl(0, 0.01), baselearner = p_fct(c("bols", "btree")))),

  bl("surv.cv_coxboost", id = "ml_gbm_coxboost", penalty = "optimCoxBoostPenalty", maxstepno = 1000),

  auto_tune(po("scale") %>>% po("encode", method = "treatment") %>>% bl("surv.svm", id = "ml_svm_van", type = "hybrid", gamma.mu = 0, diff.meth = "makediff3"),
    ps(kernel = p_fct(c("lin_kernel", "rbf_kernel")), gamma = p_dbl(1e-3, 1e3), mu = p_dbl(1e-3, 1e3))) #  FIXME: logscale?

  # TODO: 4 more learners
)

###################################################################################################
### Resampling
###################################################################################################
outer = rsmp("cv", folds = 3)

###################################################################################################
### Registry
###################################################################################################
grid = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = list(outer)
)


root = here::here()
reg_dir = file.path(root, "registry")
reg = makeExperimentRegistry(reg_dir, work.dir = root, seed = 123)
batchmark(grid, store_models = FALSE)

summarizeExperiments(by = c("task_id", "learner_id"))
unnest(getJobTable(), c("prob.pars", "algo.pars"))

# test some random jobs
ids = findNotDone()[sample(.N, 10)]
submitJobs(ids)

if (FALSE) {
  ids = findExperiments(prob.pars = task_id == "lung", algo.pars = learner_id == "class_semipar_coxph")
  ids = ajoin(ids, findDone())
  submitJobs(ids)

  ids = findExperiments(prob.pars = task_id == "lung", algo.pars = learner_id == "class_nonpar_kaplan")
  ids = ajoin(ids, findDone())
  submitJobs(ids)

  bmr = reduceResultsBatchmark()
  aggr = bmr$aggregate(conditions = TRUE)
  resamplings_with_error = aggr[errors > 0, nr]
  mlr3viz::autoplot(bmr)
  bmr$resample_result(resamplings_with_error[1])$errors


  ids = findJobs()[sample(.N, 10)]
  submitJobs(ids)
}

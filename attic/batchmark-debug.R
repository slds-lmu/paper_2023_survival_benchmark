library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")
# lgr::get_logger("mlr3")$set_threshold("debug")
# lgr::get_logger("bbotk")$set_threshold("debug")

learner = lrn(
  "surv.xgboost",
  tree_method = "hist",
  booster = "gbtree",
  objective = "survival:cox"
)

graph_learner = ppl("distrcompositor", learner = learner, form = "ph", estimator = "breslow") |>
  as_learner()

set.seed(123)
resampling = rsmp("holdout")
resampling$instantiate(tsk("grace"))

at = AutoTuner$new(
  learner = graph_learner,
  search_space = ps(
    surv.xgboost.max_depth = p_int(1, 20),
    surv.xgboost.subsample = p_dbl(0, 1),
    surv.xgboost.nrounds = p_int(10, 5000)
  ),
  resampling = resampling,
  measure = msr("surv.rcll"),
  terminator = trm("evals", n_evals = 1, k = 0),
  tuner = tnr("random_search")
)

at$train(tsk("grace"))

graph_learner$train(tsk("grace"), row_ids = resampling$instance$train)
graph_learner$predict(tsk("grace"), row_ids = resampling$instance$test)

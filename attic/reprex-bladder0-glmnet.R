# glmnet + bladder0
library(mlr3verse)
library(mlr3proba)

# Loading dataset from URL for simplicity as benchmark is in private github repo
bladder0 <- readRDS(url("https://dump.jemu.name/survdat/bladder0.rds"))

# Quick check:
dim(bladder0)
# factor var has many levels
length(unique(bladder0$Center))

# some levels don't appear for each value of status, e.g. Center == 22
bladder0 |>
  dplyr::count(status, Center) |>
  tidyr::complete(status, Center) |>
  dplyr::arrange(Center, status)

# Converting to mlr3 task, stratify by status is done for all tasks
bladder_task = as_task_surv(bladder0, target = "time", event = "status", id = "bladder0")
bladder_task$set_col_roles("status", add_to = "stratum")
bladder_task$set_col_roles("Center", add_to = "stratum")

# assemble glmnet learner without tuning
learner = lrn("surv.cv_glmnet")
learner$predict_type = "crank"

lrn_glmnet = po("fixfactors") %>>%
  # po("collapsefactors", no_collapse_above_prevalence = 0.01)
  # # not sure which is more appropriate, the latter is currently used in benchmark
  po("collapsefactors", target_level_count = 5) %>>%
  po("removeconstants") %>>%
  # encoding only done for some learners, such as glmnet as it doesn't auto-model.matrix()
  po("encode", method = "treatment") %>>%
  ppl("distrcompositor", learner = learner, form = "ph") |>
  as_learner(store_backends = TRUE)

lrn_glmnet$graph$plot()

# guess how many tries it took me to find a seed that reproduces the issue
set.seed(17)

rsmptemp = rsmp("cv", folds = 5)
rsmptemp$instantiate(bladder_task)

# Used this to find offending folds
# resample(
#   task = bladder_task,
#   learner = lrn_glmnet,
#   resampling = rsmptemp
# )

# training works
lrn_glmnet$train(bladder_task, row_ids = rsmptemp$train_set(5))
# prediction then fails
lrn_glmnet$predict(bladder_task, row_ids = rsmptemp$test_set(5))

# This is a summary of the data in that test set, notice missing factor levels and low counts
# glmnet internally does 10-fold CV so thats an issue of course
# don't know what the preprocessed dat looks like though.
bladder_task$data(rows = rsmptemp$test_set(5)) |>
  dplyr::count(status, Center) |>
  tidyr::complete(Center, status) |>
  dplyr::filter(!is.na(n))

# The task as "seen" by the learner after preprocessing(?)
lrn_glmnet$model$surv.cv_glmnet$train_task

# Can't fully access data (where would I set store_backends = TRUE?) but I get the gist.
lrn_glmnet$model$surv.cv_glmnet$train_task$data()

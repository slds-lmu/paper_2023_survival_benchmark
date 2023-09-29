# Setup ------------------------------------------------------------------
# from batchmark.R

source(here::here("settings.R"))

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
  # fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  # fallback$predict_type = "crank"
  learner$predict_type = "crank"

  # learner$fallback = fallback
  # learner$encapsulate = c(train = "evaluate", predict = "evaluate")

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


# Testruns ----------------------------------------------------------------



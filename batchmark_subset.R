root = here::here()
source(file.path(root, "settings_runtime_est.R"))
reg_dir = file.path(root, "registry_runtime_est_subset")

# Packages ----------------------------------------------------------------

# Dependencies managed via renv. Manually update as necessary via renv::update()
# See also attic/_dependencies.R
renv::restore(prompt = FALSE)

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")

#' Store nstantiated resamplings as mlr3 resampling objects
#' and as portable CSV files to `here::here("resamplings"`)
#'
#' @param resampling Object of class `Resampling`, has to be instantiated.
#' @param task_name String to use as file name.
save_resampling = function(resampling, task_name) {
  if (!dir.exists(here::here("resamplings"))) dir.create(here::here("resamplings"))
  stopifnot(resampling$is_instantiated)

  file_rds <- here::here("resamplings", paste0(task_name, ".rds"))
  file_csv <- here::here("resamplings", paste0(task_name, ".csv"))

  saveRDS(resampling, file_rds)
  write.csv(resampling$instance, file_csv, row.names = FALSE)
}

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

    save_resampling(resampling, names[i])
  }



  tasks[[i]] = task
  resamplings[[i]] = resampling
  rm(data, task, folds, resampling)
}

# Save overview of tasks with some metadata which comes in handy later
tasktab = data.table::rbindlist(lapply(tasks, \(x) {
  data.table::data.table(
    task_id = x$id,
    n = x$nrow,
    p = length(x$feature_names),
    dim = x$nrow * length(x$feature_names),
    n_uniq_t = length(unique(x$data(cols = "time")[[1]]))
  )
}))
tasktab[, dimrank := data.table::frank(dim)]
tasktab[, uniq_t_rank := data.table::frank(n_uniq_t)]

write.csv(tasktab, here::here("tasktab.csv"), row.names = FALSE)
#saveRDS(tasktab, file = here::here("tasktab.rds"))

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

auto_tune = function(learner, ...) { # wrap into random search
  learner = as_learner(learner)
  search_space = ps(...)
  if (is.null(search_space$trafo))
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))

  if (inner_folds == 1) {
    # Runtime testing mode: holdout
    resampling = rsmp("holdout")
  } else {
    # regular mode: (3-fold) CV
    resampling = rsmp("cv", folds = inner_folds)
  }

  AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = resampling,
    # Measure will be set via global variable in loop
    measure = measure,
    # budget set in settings.R: n_evals + k * dim_search_space
    terminator = trm("evals", n_evals = budget_constant, k = budget_multiplier),
    tuner = tnr("random_search"),
    store_tuning_instance = TRUE,
    store_benchmark_result = FALSE,
    store_models = FALSE
  )
}

# Set tuning measures -----------------------------------------------------
# Tuning measures are a subset of all measures, remaining measures are used
# for evaluation (see overleaf Table 1)
measures = list(
  msr("surv.cindex", id = "harrell_c"),
  msr("surv.dcalib", id = "dcalib"),

  # Added as graf alternative for now as per RS
  msr("surv.rcll", id = "rcll")

  # If graf, then both
  # msr("surv.graf", id = "graf_proper", proper = TRUE),
  # msr("surv.graf", id = "graf_improper", proper = FALSE)
)


# Asssemle learners -------------------------------------------------------
for (measure in measures) {
  learners = list(
    KM = bl("surv.kaplan")

    ,

    NL = bl("surv.nelson")

    ,

    # survivalmodels::akritas
    # https://raphaels1.github.io/survivalmodels/reference/akritas.html
    AF = auto_tune(
      bl("surv.akritas"),
      surv.akritas.lambda = p_dbl(0, 1)
    )

    ,

    CPH = bl("surv.coxph")

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

    XGB = auto_tune(
      bl("surv.xgboost", tree_method = "hist", booster = "gbtree", .encode = TRUE),
      surv.xgboost.max_depth = p_int(1, 20),
      surv.xgboost.subsample = p_dbl(0, 1),
      surv.xgboost.colsample_bytree = p_dbl(0, 1),
      surv.xgboost.nrounds = p_int(10, 5000),
      surv.xgboost.eta = p_dbl(0, 1),
      surv.xgboost.grow_policy = p_fct(c("depthwise", "lossguide"))
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

print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")
alljobs = ljoin(alljobs, tasktab, by = "task_id")
data.table::setkey(alljobs, job.id)


submitJobs(findNotSubmitted())

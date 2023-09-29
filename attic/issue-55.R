# Issue 46 -> 55

# number of outer (evaluation) folds
outer_folds = 5

# tuning budget
budget_constant = 3
budget_multiplier = 0

# number of inner (tuning) folds
inner_folds = 3

# RNG seed
seed = 123

###################################################################################################
### Packages ----
###################################################################################################

#library("stringi")
library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("dplyr")
requireNamespace("mlr3extralearners")

set.seed(seed)

# From Raphael's preprocessing code
aids2 <- MASS::Aids2 %>%
  mutate(time = as.numeric(death - diag),
         sexF = if_else(sex == "F", 1L, 0L),
         status = if_else(status == "D", 1L, 0L),
         age = as.numeric(age)) %>%
  select(-diag, -death, -sex) %>%
  tidyr::drop_na() %>%
  filter(time > 0)


# Stripped back batchmark code
task = as_task_surv(aids2, target = "time", event = "status", id = "aids2")
task$set_col_roles("status", add_to = "stratum")
resampling = rsmp("cv", folds = outer_folds)$instantiate(task)

# Create Learners and populate Registry -----------------------------------
bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  # fallback disabled during debugging
  #fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  #fallback$predict_type = "crank"
  learner$predict_type = "crank"

  # Added form as per RS
  g = ppl("distrcompositor", learner = learner, form = 'ph')

  as_learner(po("fixfactors") %>>% po("collapsefactors", target_level_count = 5) %>>% g)
}

set.seed(123)
Par = AutoTuner$new(
  learner = as_learner(bl("surv.parametric", type = "aft")),
  search_space = ps(surv.parametric.dist = p_fct(c("weibull", "lognormal", "loglogistic"))),
  resampling = rsmp("cv", folds = inner_folds),
  measure = msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  terminator = trm("evals", n_evals = budget_constant, k = budget_multiplier),
  tuner = tnr("random_search"),
  store_tuning_instance = TRUE,
  store_benchmark_result = FALSE,
  store_models = TRUE # Preserve models for debugging
)

#Par$train(task, row_ids = resampling$train_set(3))

rr <- resample(
  task,
  Par,
  resampling
)



# Task strata test --------------------------------------------------------

# Added stratification
task$set_col_roles("T.categ", add_to = "stratum")

# pl <- po("fixfactors") %>>% po("collapsefactors", target_level_count = 5)
pl <- po("fixfactors") %>>% po("collapsefactors", no_collapse_above_prevalence = 0.01)
tt2 <- pl$train(task)

sort(table(tt2$collapsefactors.output$data(cols = "T.categ")))
sort(table(task$data(cols = "T.categ")))

skimr::skim(tt2$collapsefactors.output$data())
skimr::skim(task$data())

task$properties
task$col_roles
task$strata

dd <- task$data()
table(dd$status, dd$T.categ)
task$strata


## seperate issue: fixed in gh version
library(mlr3)
tt <- tsk("iris")
tt$set_col_roles("Species", add_to = "stratum")
tt

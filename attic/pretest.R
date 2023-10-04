source(here::here("settings_debug.R"))

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
library("batchtools")
library("mlr3batchmark")
requireNamespace("mlr3extralearners")

# Assumes batchmark.R is run beforehand
reg = loadRegistry(reg_dir, writeable = TRUE)

# All jobs ----------------------------------------------------------------

alljobs = unnest(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

alljobs[, .(count = .N), by = task_id]
alljobs[, .(count = .N), by = .(task_id, learner_id, measure)]

# Factor "missing" issue --------------------------------------------------
# Happens independently of dummy encoding yes/no

# Error : Task 'bladder0' has missing values in column(s) 'Center', but learner 'surv.akritas' does not support this This happened PipeOp surv.akritas's $predict()
# Error : Task 'hdfail' has missing values in column(s) 'model', but learner 'surv.akritas' does not support this This happened PipeOp surv.akritas's $predict()
# Error : Task 'hdfail' has missing values in column(s) 'model', but learner 'surv.coxph' does not support this This happened PipeOp surv.coxph's $predict()
# Error : Task 'whas' has missing values in column(s) 'mitype', but learner 'surv.akritas' does not support this This happened PipeOp surv.akritas's $predict()
# Error : Task 'aids2' has missing values in column(s) 'T.categ', but learner 'surv.parametric' does not support this This happened PipeOp surv.parametric's $predict()
# Error : Task 'aids2' has missing values in column(s) 'T.categ', but learner 'surv.flexible' does not support this This happened PipeOp surv.flexible's $predict()
# Error : Task 'bladder0' has missing values in column(s) 'Center.336', 'Center.525', 'Center.533', 'Center.710', but learner 'surv.cv_glmnet' does not support this This happened PipeOp surv.cv_glmnet's $predict()
# Error : Task 'bladder0' has missing values in column(s) 'Center.525', 'Center.533', 'Center.710', 'Center.903', but learner 'surv.cv_glmnet' does not support this This happened PipeOp surv.cv_glmnet's $predict()
# Error : Task 'bladder0' has missing values in column(s) 'Center', but learner 'surv.ranger' does not support this This happened PipeOp surv.ranger's $predict()
# Error : Task 'hdfail' has missing values in column(s) 'model.Hitachi.HDS5C3030ALA630', 'model.Hitachi.HDS722020ALA330', 'model.ST3000DM001', 'model.ST4000DM000', but learner 'surv.cv_glmnet' does not support this This happened PipeOp surv.cv_glmnet's $predict()

problem_tasks <- c("bladder0", "hdfail", "whas", "aids2")
problem_learners <- c("AF","CPH", "GLM", "Par", "Flex", "CoxB", "RAN")

problem_jobs <- alljobs[task_id %in% problem_tasks & learner_id %in% problem_learners & grepl("rcll", measure)]
nrow(problem_jobs)

problem_jobs <- ijoin(problem_jobs, findExperiments(repls = 1))
nrow(problem_jobs)

problem_jobs |>
  findNotSubmitted() |>
  submitJobs(resources = resources)

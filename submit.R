root = here::here()
source(file.path(root, "settings.R"))
# reg_dir = file.path(root, "registry_runtime_est")

library("batchtools")
library("mlr3batchmark")


# Assumes batchmark.R is run beforehand
reg = loadRegistry(reg_dir, writeable = TRUE)

print(summarizeExperiments(by = c("task_id", "learner_id")))

# Aggregate job table for selective submission, order jobs by tasks and taks
# by number of unique time points (ranked) (higher == more memory needed)
alljobs = unwrap(getJobTable(), c("prob.pars", "algo.pars"))[, .(job.id, repl, tags, task_id, learner_id)]
data.table::setnames(alljobs, "tags", "measure")

tasktab = read.csv(here::here("attic/tasktab.csv"))
resource_tab = read.csv(here::here("attic/resource_est.csv"))
resource_tab = resource_tab[, c("learner_id", "task_id", "measure", "hours", "mem_gb")]

alljobs = ljoin(alljobs, tasktab, by = "task_id")
alljobs = ljoin(alljobs, resource_tab, by = c("task_id", "learner_id", "measure"))

data.table::setkey(alljobs, job.id)

alljobs = alljobs[learner_id == "XGB", ]

alljobs[, chunk := lpt(hours, 10)]
print(alljobs[, list(runtime = sum(hours), mem = sum(mem_gb), count = .N), by = chunk])


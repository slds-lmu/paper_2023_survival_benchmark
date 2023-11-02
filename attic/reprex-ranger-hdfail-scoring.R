library(mlr3)
library(mlr3extralearners)
library(mlr3proba)
library(tictoc)

# All datasets are available at that URL in their preprocessed form
hdfail = readRDS(url("https://dump.jemu.name/survdat/hdfail.rds"))
# child = readRDS(url("https://dump.jemu.name/survdat/child.rds")) #other large dataset


task = as_task_surv(hdfail, time = "time", event = "status")
task$set_col_roles("status", add_to = "stratum")

learner = lrn("surv.ranger", num.trees = 100)
learner$predict_type = "crank"

set.seed(3)
split = partition(task)

tic(msg = "Train")
learner$train(task, row_ids = split$train)
toc()

tic(msg = "Predict")
pred = learner$predict(task, row_ids = split$test)
toc()

tic(msg = "Score: RCLL")
pred$score(msr("surv.rcll"))
toc()

tic(msg = "Score: D-Calib")

pred$score(msr("surv.dcalib"))
toc()

tic(msg = "Score: Harrell's C")
pred$score(msr("surv.cindex"))
toc()

# Try to see if DL learners function somewhat properly
# Python dependency hell may be an issue

# Creates learners, loads task etc. but does not to batchtools stuff
source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# surv.dnnsurv: R-keras
learners[["DNN"]]$train(tasks$mgus)
learners[["DNN"]]$predict(tasks$mgus)
learners[["DNN"]]$errors
learners[["DNN"]]$warnings

# surv.deepsurv: R-keras
learners[["DS"]]$train(tasks$mgus)
learners[["DS"]]$predict(tasks$mgus)
learners[["DS"]]$errors
learners[["DS"]]$warnings

# surv.coxtime: pycox
learners[["CoxT"]]$train(tasks$mgus)
learners[["CoxT"]]$predict(tasks$mgus)
learners[["CoxT"]]$errors
learners[["CoxT"]]$warnings

# surv.pchazard: PyTorch
learners[["PCH"]]$train(tasks$mgus)
learners[["PCH"]]$predict(tasks$mgus)
learners[["PCH"]]$errors
learners[["PCH"]]$warnings

# surv.deephit: PyTorch
learners[["DH"]]$train(tasks$mgus)
learners[["DH"]]$predict(tasks$mgus)
learners[["DH"]]$errors
learners[["DH"]]$warnings

# surv.loghaz: PyTorch
learners[["LH"]]$train(tasks$mgus)
learners[["LH"]]$predict(tasks$mgus)
learners[["LH"]]$errors
learners[["LH"]]$warnings

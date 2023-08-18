# Try to see if DL learners function somewhat properly
# Python dependency hell may be an issue

# Set measure = msr("surv.rcll", id = "rcll") for testing
source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# surv.dnnsurv: R-keras, not available on arm64 without conda, but conda with renv is hard sorry
# appears to work
learners[["DNN"]]$train(tasks$mgus)
learners[["DNN"]]$predict(tasks$mgus)
learners[["DNN"]]$errors
learners[["DNN"]]$warnings

# surv.deepsurv: R-keras
# FIXME: ImportError: Numba needs NumPy 1.24 or less
learners[["DS"]]$train(tasks$mgus)
learners[["DS"]]$predict(tasks$mgus)
learners[["DS"]]$errors
learners[["DS"]]$warnings

# surv.coxtime: pycox
# FIXME:  ImportError: Numba needs NumPy 1.24 or less
learners[["CoxT"]]$train(tasks$mgus)
learners[["CoxT"]]$predict(tasks$mgus)
learners[["CoxT"]]$errors
learners[["CoxT"]]$warnings

# surv.pchazard: PyTorch
# FIXME: ImportError: Numba needs NumPy 1.24 or less
learners[["PCH"]]$train(tasks$mgus)
learners[["PCH"]]$predict(tasks$mgus)
learners[["PCH"]]$errors
learners[["PCH"]]$warnings

# surv.deephit: PyTorch
# FIXME: ImportError: Numba needs NumPy 1.24 or less
learners[["DH"]]$train(tasks$mgus)
learners[["DH"]]$predict(tasks$mgus)
learners[["DH"]]$errors
learners[["DH"]]$warnings

# surv.loghaz: PyTorch
# FIXME: ImportError: Numba needs NumPy 1.24 or less
learners[["LH"]]$train(tasks$mgus)
learners[["LH"]]$predict(tasks$mgus)
learners[["LH"]]$errors
learners[["LH"]]$warnings

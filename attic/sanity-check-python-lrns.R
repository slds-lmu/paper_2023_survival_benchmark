# Try to see if DL learners function somewhat properly
# Python dependency hell may be an issue

# Set measure = msr("surv.rcll", id = "rcll") for testing
source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# surv.dnnsurv: R-keras, not available on arm64 without conda, but conda with renv is hard sorry
learners[["DNN"]]$train(tasks$mgus)
learners[["DNN"]]$errors
learners[["DNN"]]$warnings

# surv.deepsurv: R-keras
learners[["DS"]]$train(tasks$mgus)
learners[["DS"]]$errors
learners[["DS"]]$warnings

# surv.coxtime: pycox
# Can likely be ignored?
# ./renv/python/virtualenvs/renv-python-3.10/lib/python3.10/site-packages/pycox/models/data.py:38:
# FutureWarning: iteritems is deprecated and will be removed in a future version. Use .items instead.
# for ix, t in keys.iteritems():
learners[["CoxT"]]$train(tasks$mgus)
learners[["CoxT"]]$errors
learners[["CoxT"]]$warnings

# surv.pchazard: PyTorch
learners[["PCH"]]$train(tasks$mgus)
learners[["PCH"]]$errors
learners[["PCH"]]$warnings

# surv.deephit: PyTorch
learners[["DH"]]$train(tasks$mgus)
learners[["DH"]]$errors
learners[["DH"]]$warnings

# surv.loghaz: PyTorch
learners[["LH"]]$train(tasks$mgus)
learners[["LH"]]$errors
learners[["LH"]]$warnings

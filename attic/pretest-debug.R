# Setup learners/tasks/resamplings
source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# Store eval measures for easier retrieval
measures_eval = list(
  harrell_c = msr("surv.cindex", id = "harrell_c"),
  uno_c = msr("surv.cindex", id = "uno_c", weight_meth = "G2"),
  # Added as graf alternative for now as per RS
  rcll = msr("surv.rcll", id = "rcll"),
  # msr("surv.graf", id = "graf", proper = TRUE),
  dcalib = msr("surv.dcalib", id = "dcalib"),

  intlogloss = msr("surv.intlogloss", id = "intlogloss", proper = TRUE),
  logloss = msr("surv.logloss", id = "logloss"),
  calib = msr("surv.calib_alpha", id = "calib")
)


# Par / CarpenterFdaData / rcll -------------------------------------------
# https://github.com/RaphaelS1/proba_benchmark/issues/47
rr = resample(
  task = tasks$CarpenterFdaData,
  learner = learners$Par,
  resampling = resamplings$CarpenterFdaData
)
# Seems to run and eval fine?
rr$score(measures = measures_eval[c("harrell_c", "rcll")])

# glmnet on mgus ----------------------------------------------------------
rr = resample(
  task = tasks$mgus,
  learner = learners$GLM,
  resampling = resamplings$mgus
)

# harrell_c seems plausible? > 0.6 at least. rcll > 30 though.
rr$score(measures = measures_eval[c("harrell_c", "rcll")])


# glmnet / wbc1 -----------------------------------------------------------
rr = resample(
  task = tasks$wbc1,
  learner = learners$GLM,
  resampling = resamplings$wbc1
)

# C's are 0.5
rr$aggregate(measure = measures_eval)
rr$score(measures = measures_eval)

# Task only has two features though
tasks$wbc1$feature_names

# Single fold predictions are crank == lp == 0
debuglrn = learners$GLM$clone()
debuglrn$train(tasks$wbc1, row_ids = resamplings$wbc1$train_set(1))
debuglrn$predict(tasks$wbc1, row_ids = resamplings$wbc1$test_set(1))

# is glmnet shrinking away the only predictors? o_O
debuglrn$tuning_instance$result
debuglrn$model$learner$model$surv.cv_glmnet$model$lambda
debuglrn$model$learner$model$surv.cv_glmnet$model$glmnet.fit$beta
# looks like it?
coef(debuglrn$model$learner$model$surv.cv_glmnet$model)

# Compare with coxph
debugcph = learners$CPH$clone()
debugcph$train(tasks$wbc1, row_ids = resamplings$wbc1$train_set(1))
debugcph$predict(tasks$wbc1, row_ids = resamplings$wbc1$test_set(1))

# What does the vanilla learner do?
# Also predicts crank == lp == 0
glmn <- lrn("surv.cv_glmnet")
glmn$train(tasks$wbc1, row_ids = resamplings$wbc1$train_set(1))
glmn$predict(tasks$wbc1, row_ids = resamplings$wbc1$test_set(1))
# Coefs for chosen lambda are nonzero
coef(glmn$model, s = "lambda.min")

# Manual predictions give nonzero values
wbc1_test = as.matrix(tasks$wbc1$data())[resamplings$wbc1$test_set(1), 3:4]
predict(glmn$model, newx = wbc1_test, s = "lambda.min")


# ORSF / multiple tasks -------------------------------------------------------------
# Error: mtry = 1 should be >= 2
# This happened PipeOp surv.aorsf's $train()

# Affects tasks: wbc1, ALL, CarpenterFdaData, aids.id, aids2, channing
# Makes sense that a p = 2 dataset won't do well here.
# Likely masked by fallback learner?

rr = resample(
  task = tasks$wbc1,
  learner = learners$ORSF,
  resampling = resamplings$wbc1
)


# LH / lung ---------------------------------------------------------------

rr = resample(
  task = tasks$lung,
  learner = learners$LH,
  resampling = resamplings$lung
)

# C < 0.5 in first iteration
rr$score(measures = measures_eval[c("harrell_c", "rcll")])


# PCH / lung --------------------------------------------------------------

rr = resample(
  task = tasks$lung,
  learner = learners$PCH,
  resampling = resamplings$lung
)

rr$score(measures = measures_eval[c("harrell_c", "rcll")])


# LH / lung ---------------------------------------------------------------

rr = resample(
  task = tasks$lung,
  learner = learners$LH,
  resampling = resamplings$lung
)

# C around 0.5, rcll > 25
rr$score(measures = measures_eval[c("harrell_c", "rcll")])


# SVM / CarpenterFdaData --------------------------------------------------

rr = resample(
  task = tasks$CarpenterFdaData,
  learner = learners$SSVM,
  resampling = resamplings$CarpenterFdaData
)
# Error in quadprog::solve.QP(C, -d, t(H), f, meq = meq) :
# constraints are inconsistent, no solution!
#  This happened PipeOp surv.svm's $train()


# SVM / ALL ---------------------------------------------------------------
# pretest job shows harrell's C 0.5 but rcll 7.432

rr$score(measures = measures_eval[c("harrell_c", "rcll")])

rr = resample(
  task = tasks$ALL,
  learner = learners$SSVM,
  resampling = resamplings$ALL
)

rr$score(measures = measures_eval[c("harrell_c", "rcll")])


# Pycox censoring at start time error -------------------------------------
# Affects ALL, CarpenterFdaData, aids.id

learners$PCH$train(tasks$ALL, row_ids = resamplings$ALL$train_set(1))

learners$PCH$train(tasks$CarpenterFdaData, row_ids = resamplings$CarpenterFdaData$train_set(1))

learners$PCH$train(tasks$aids.id, row_ids = resamplings$aids.id$train_set(1))

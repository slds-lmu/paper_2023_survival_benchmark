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

rr = resample(
  task = tasks$aids2,
  learner = learners$Par,
  resampling = resamplings$aids2,
)


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




# Pycox censoring at start time error -------------------------------------
# Affects ALL, CarpenterFdaData, aids.id

learners$PCH$train(tasks$ALL, row_ids = resamplings$ALL$train_set(1))

learners$PCH$train(tasks$CarpenterFdaData, row_ids = resamplings$CarpenterFdaData$train_set(1))

learners$PCH$train(tasks$aids.id, row_ids = resamplings$aids.id$train_set(1))

learners$PCH$train(tasks$e1684, row_ids = resamplings$e1684$train_set(1))
learners$PCH$train(tasks$liver, row_ids = resamplings$liver$train_set(1))

learners$PCH$reset

learners$PCH$train(tasks$aids2, row_ids = resamplings$aids2$train_set(1))



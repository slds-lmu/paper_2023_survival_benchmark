source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# Resampling with learners/tasks/instantiated resamplings from batchmark.R
rr = resample(
  task = tasks$mgus,
  learner = learners$GLM,
  resampling = resamplings$mgus
)

debuglrn = learners$GLM$clone()
debuglrn$train(tasks$mgus, row_ids = resamplings$mgus$train_set(4))

source(here::here("attic/batchmark-mini.R"), echo = FALSE)

# Resampling with learners/tasks/instantiated resamplings from batchmark.R
rr = resample(
  task = tasks$mgus,
  learner = learners$GLM,
  resampling = resamplings$mgus
)

rr

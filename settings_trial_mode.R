###################################################################################################
### Benchmark Settings
###################################################################################################
# registry dir (NA for testing)
reg_dir = file.path(root, "registry")

# number of outer (evaluation) folds
outer_folds = 5

# minimum number of observations per fold. If this requirement is not met,
# outer_folds is decreased to the largest fold number where this is possible.
min_obs = 30

# Trial mode budget
# budget_constant = 5
# budget_multiplier = 1

# Suggested budget
budget_constant = 10
budget_multiplier = 0

# Maximum time autotuner is allowed to run, scale with budget?
budget_runtime_seconds = 3600

# number of inner (tuning) folds
inner_folds = 3

timeout_train = 600
timeout_predict = 600

# RNG seed
seed = 123

# resources
resources_default = list(
  walltime = 10 * 3600,
  memory = 10 * 1024,
  measure.memory = TRUE
)

resources_long = list(
  walltime = 10 * 24 * 3600,
  memory = 50 * 1024,
  measure.memory = TRUE
)

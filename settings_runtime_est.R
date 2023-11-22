###################################################################################################
### Benchmark Settings
###################################################################################################
# registry dir (NA for testing)
reg_dir = file.path(root, "registry_runtime_est")

# number of outer (evaluation) folds
outer_folds = 1

# minimum number of observations per fold. If this requirement is not met,
# outer_folds is decreased to the largest fold number where this is possible.
min_obs = 30

# Trial mode budget
# budget_constant = 5
# budget_multiplier = 1

# Suggested budget
budget_constant = 1
budget_multiplier = 0

# number of inner (tuning) folds
inner_folds = 1

# RNG seed
seed = 123

# resources
resources = list(
  walltime = 2 * 24 * 3600,
  memory = 60 * 1024,
  measure.memory = TRUE
)

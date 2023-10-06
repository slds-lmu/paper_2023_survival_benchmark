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

# tuning budget
budget_constant = 3
budget_multiplier = 0
# budget_constant = 0
# budget_multiplier = 50

# number of inner (tuning) folds
inner_folds = 3

# RNG seed
seed = 123

# resources
resources = list(walltime = 3600, memory = 8 * 1024)

###################################################################################################
### Benchmark Settings
###################################################################################################
# registry dir (NA for testing)
reg_dir = file.path(root, "registry")

# number of outer cv folds
outer_folds = 5

# minimum number of observations per fold. If this requirement is not met, 
# outer_folds is decreased to the largest fold number where this is possible.
min_obs = 30

# RNG seed 
seed = 123

# resources
resources = list(walltime = 3600, memory = 8 * 1024)

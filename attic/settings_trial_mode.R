###################################################################################################
### Benchmark Settings
###################################################################################################
# registry dir (NA for testing)
reg_dir = file.path(root, "registry_trial")

use_fallback_inner = FALSE
use_fallback_outer = FALSE

# number of outer (evaluation) folds
outer_folds = 5

# number of inner (tuning) folds
inner_folds = 3

# minimum number of observations per fold. If this requirement is not met,
# outer_folds is decreased to the largest fold number where this is possible.
min_obs = 30

# RNG seed
seed = 123

# Budgets -------------------------------------------------------------------------------------

# Trial mode budget
# budget_constant = 5
# budget_multiplier = 1

# Suggested budget
budget_constant = 0
budget_multiplier = 50

# Maximum time autotuner is allowed to run for one set of inner resamplings
budget_runtime_seconds = 3600 * 1

# Timeout for GraphLearner, on one inner resampling fold
timeout_train_bl = 3600 * 5
timeout_predict_bl = 3600 * 5

# AutoTuner, limited by cluster job walltime, 1 job == 1 outer resample
# Leaving 1 hour headroom to assemble everything
timeout_train_at = 3600 * 24
timeout_predict_at = 3600 * 24

# Resources -----------------------------------------------------------------------------------

resources_default = list(
  walltime = 10 * 3600,
  memory = 10 * 1024,
  measure.memory = TRUE
)

resources_long = list(
  walltime = 7 * 24 * 3600,
  memory = 50 * 1024,
  measure.memory = TRUE
)

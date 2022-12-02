source("renv/activate.R")

# Manual python/conda activation
reticulate::use_condaenv("proba-bench", required = TRUE)

# Trying to ensure learners don't use more resources than they should
Sys.setenv(OMP_NUM_THREADS = "1")
Sys.setenv(OPENBLAS_NUM_THREADS = "1")
Sys.setenv(MKL_NUM_THREADS = "1")

options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)

source("renv/activate.R")

# Manual python/conda activation
# Disabled on arm due to conda issues for now

# Disabled globally for now for debugging purposes

if (Sys.info()[["machine"]] != "arm64") {
  reticulate::use_condaenv("proba-bench3.10", required = TRUE)
}
print(reticulate::py_config())

# Trying to ensure learners don't use more resources than they should
Sys.setenv(OMP_NUM_THREADS = "1")
Sys.setenv(OPENBLAS_NUM_THREADS = "1")
Sys.setenv(MKL_NUM_THREADS = "1")

options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE,
  batchtools.progress = FALSE
)

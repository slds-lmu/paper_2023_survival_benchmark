# A Large-Scale Neutral Comparison Study of Survival Models on Low-Dimensional Data

## Setup

The benchmark is conducted using R and the `mlr3` framework. The following files are necessary to set up the benchmark:

- `config.yml` contains configuration defaults for different settings (pretests, debugging, etc.)
  - It is loaded using `config::get()` in the main scripts.
- `batchmark.R` is the main setup script. It creates a benchmark registry (using `batchtools` and `mlr3batchmark`).
  - Resulting compute jobs can be submitted with `batchtools::submitJobs()` depending on available resources.
  - `submit.R` contains job management steps specific to the HPC environment used for the benchmark.
- Datasets are loaded from `./datasets` in `.rds` format
  - Code for retrieval and minimal preprocessing is in `import_new_data.R`
- `resamplings` contains resampling fold information for each dataset in CSV form to ensure reproducibility.
  - These files are automatically generated when running `batchmark.R`, where resampling is done using a set RNG seed.
- `helpers.R` contains helper functions for benchmark setup and later analysis.
- `analysis.qmd` contains the main analysis steps including additional exploratory analysis.

## Reproducibility

- `renv` and `renv.lock` contain [renv](https://github.com/r-lib/renv) project information.
  - `renv::restore()` can be used to restore the project environment.
  - Due to limitations on the HPC environment used for the benchmark, R version 4.2.2 is expected.
  (you may want to use [rig]([renv](https://github.com/r-lib/rig) for R version management)


## Results

Results are available online at [projects.lukasburk.de/survival_benchmark/](https://projects.lukasburk.de/survival_benchmark/index.html)

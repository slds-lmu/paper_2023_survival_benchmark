# A Large-Scale Neutral Comparison Study of Survival Models on Low-Dimensional Data

## Setup

The benchmark is conducted using R and the `mlr3` framework. The following files are necessary to set up the benchmark:

- `config.yml` contains configuration defaults for different settings (pretests, debugging, etc.)
  - It is loaded using `config::get()` in the main scripts.
- `batchmark.R` is the main setup script. It creates a benchmark registry (using `batchtools` and `mlr3batchmark`).
  - Resulting compute jobs can be submitted with `batchtools::submitJobs()` depending on available resources.
  - `submit.R` contains job management steps specific to the HPC environment used for the benchmark.
  - To actually run the benchmark, you need to adjust the `submit.R` script to your environment! Please refer to the `batchtools` documentation for more information.
- Datasets are loaded from `./datasets` in `.rds` format, where they are also stored in CSV and arff formats.
  - Code for retrieval and minimal preprocessing is in `import_datasets.R`
- `resamplings` contains resampling fold information for each dataset in CSV form to ensure reproducibility.
  - These files are automatically generated when running `batchmark.R`, where resampling is done using a set RNG seed.
- `helpers.R` contains helper functions for benchmark setup and later analysis.
- `analysis.qmd` contains the main analysis steps including additional exploratory analysis.

## Reproducibility

- `renv` and `renv.lock` contain [renv](https://github.com/r-lib/renv) project information.
  - `renv::restore()` needs to be used to restore the project environment.
  - Due to limitations on the HPC environment used for the benchmark, R version 4.2.2 is expected.
  (you may want to use [rig]([renv](https://github.com/r-lib/rig) for R version management)
  - Additionally, the `PMCMRplus` is required by `mlr3benchmark`, but was not installable in the HPC environment, so it is not included by `renv` and you may need to install it manually from CRAN: `install.packages("PMCMRplus")`.
- `produce_paper_plots.R` contains code to reproduce the plots used in the paper and aims to be as self-contained as possible, but loads helper functions from `helpers.R` for de-duplication and readability.
  - The output path is `./results_paper` by default.
  
Please note that due to the large file sizes of the `BenchmarkResult` (`bmr`) objects produced by the aggregation of the `batchtools` registry, this repository only contains the processed result files (`results/registry_beartooth/`) required to produce the main results of the paper.


## Results

Results are available online at [projects.lukasburk.de/survival_benchmark/](https://projects.lukasburk.de/survival_benchmark/index.html)

The site is generated from the quarto site in `/site/`.

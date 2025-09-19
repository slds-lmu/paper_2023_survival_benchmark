# A Large-Scale Neutral Comparison Study of Survival Models on Low-Dimensional Data

> L. Burk, J. Zobolas, B. Bischl, A. Bender, M. N. Wright, and R. Sonabend, “A Large-Scale Neutral Comparison Study of Survival Models on Low-Dimensional Data.” arXiv, Jun. 06, 2024. doi: [10.48550/arXiv.2406.04098](https://arxiv.org/abs/2406.04098).

:warning: A note on versioning: This repository is actively in development. To view its state at the time of submission of the arXiv preprint, please [browse this tag on GitHub](https://github.com/slds-lmu/paper_2023_survival_benchmark/tree/f6c36cd43f1705e3612ff4f47472327a3d29ebe8)

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
  - In `batchmark.R`, the datasets are loaded and converted into `{mlr3}` `TaskSurv` objects.
- `resamplings` contains resampling fold information for each dataset in CSV form to ensure reproducibility.
  - These files are automatically generated when running `batchmark.R`, where resampling is done using a set RNG seed.
- `helpers.R` contains helper functions for benchmark setup and later analysis.
- `site/` contains a quarto site with all results.

## Reproducibility

- `renv` and `renv.lock` contain [renv](https://github.com/r-lib/renv) project information.
  - `renv::restore()` needs to be used to restore the project environment.
  - Start by restoring `mlr3extralearners`, then `mlr3` and then continue with the remaining packages:

  ```r
  renv::restore(exclude= c("mlr3", "mlr3extralearners", "mlr3proba"))
  renv::restore(packages = "mlr3extralearners")
  renv::restore(packages = "mlr3")
  renv::restore()
  ```
      
  - Due to limitations on the HPC environment used for the benchmark, R version 4.4.3 is expected.
  (you may want to use [rig](https://github.com/r-lib/rig) for R version management)
- `produce-paper-plots.R` contains code to reproduce the plots used in the paper and aims to be as self-contained as possible
  - The output path is `./results_paper` by default.
  
Please note that due to the large file sizes of the `BenchmarkResult` (`bmr`) objects produced by the aggregation of the `batchtools` registry, this repository only contains the processed result files (`./results/registry_beartooth/`) required to produce the main results of the paper.


## Results

Results are available online at [projects.lukasburk.de/survival_benchmark/][quarto_site]

The site is generated from the [quarto][quarto] site in `./site/`.

## Datasets

The datasets used in the benchmark are stored after minor modifications in `./datasets/` and are also uploaded to [OpenML][openml].
The dataset's names, source package, and OpenML dataset IDs are stored in `./dataset_table.[csv|rds]`.

Here is a short example on how to download the datasets from OpenML using [`{mlr3oml}`][mlr3oml]:

```r
# Get dataset from openml
library(mlr3oml)
library(mlr3proba) # To create survival tasks via as_task_surv

# The 'qs' package is required for caching the downloaded data
if (requireNamespace("qs", quietly = TRUE)) {
  options(mlr3oml.cache = TRUE)
}

# Get the table of datasets & their OpenML IDs
dataset_tbl = readRDS(here::here("dataset_table.rds"))
head(dataset_tbl[, c("dataset", "dataset_id")])
```
```
   dataset dataset_id
1     gbsg      46131
2 metabric      46142
3  support      46144
4   colrec      46145
5    rdata      46146
6  aids.id      46130
```

```r
# Get an individual dataset in the OMLData class
colrec_odt = mlr3oml::odt(46145)

# Convert the OMLData object to a TaskSurv object in a loop, creating a list of mlr3 TaskSurv objects
task_list = lapply(dataset_tbl$dataset_id, function(id) {
  dat = mlr3oml::odt(id)

  task = mlr3proba::as_task_surv(mlr3::as_data_backend(dat), target = "time", event = "status", id = dat$name)
  task$set_col_roles("status", add_to = "stratum")
  Sys.sleep(0.1) # Small timeout to not hammer the OML server
  task
})

task_list[[1]]
```
```
<TaskSurv:gbsg> (2232 x 9)
* Target: time, status
* Properties: strata
* Features (7):
  - int (7): x0, x1, x2, x3, x4, x5, x6
* Strata: status
```

[quarto_site]: https://projects.lukasburk.de/survival_benchmark/index.html
[quarto]: https://quarto.org/
[openml]: https://www.openml.org/
[mlr3oml]: https://github.com/mlr-org/mlr3oml

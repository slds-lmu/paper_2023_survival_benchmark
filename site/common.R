knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE, dev = "ragg_png")

source(here::here("R/helpers.R"))

settings = config::get(config = "beartooth")
reg_dir = here::here(settings$reg_name)
result_path = here::here("results")

###################################################################################################
### Packages
###################################################################################################
library(mlr3benchmark)
library(mlr3proba)
requireNamespace("mlr3extralearners")
# requires PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(ggplot2)
library(kableExtra)
library(dplyr)

result_path = here::here("results", settings$reg_name)
msr_tbl = measures_tbl()
measures_eval_ids = msr_tbl$id

lrntab = load_lrntab()
tasktab = load_tasktab()

# bma is the BenchmarkAggr for use with mlr3benchmark
bma_harrell_c = readRDS(fs::path(result_path, "bma_clean_harrell_c.rds"))
bma_isbs      = readRDS(fs::path(result_path, "bma_clean_isbs.rds"))

bmrtab_harrell_c = readRDS(fs::path(result_path, "bmrtab_harrell_c.rds"))
bmrtab_isbs = readRDS(fs::path(result_path, "bmrtab_isbs.rds"))

aggr_scores = readRDS(fs::path(result_path, "aggr_scores.rds"))
scores = readRDS(fs::path(result_path, "scores.rds"))

# Create the scaled version of aggregated scores where KM is 0 and best model is 1
aggr_scores_scaled = rescale_aggr_scores(aggr_scores, msr_tbl)

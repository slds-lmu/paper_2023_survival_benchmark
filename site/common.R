knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE, dev = "ragg_png")

source(here::here("R/helpers.R"))
source(here::here("R/plotting.R"))

conf = config::get(config = "production", file = here::here("config.yml"))
result_path = conf$result_path

###################################################################################################
### Packages
###################################################################################################
library(mlr3benchmark)
library(mlr3proba)
requireNamespace("mlr3extralearners")
library(ggplot2)
library(kableExtra)
library(dplyr)

msr_tbl = measures_tbl()

lrntab = load_lrntab()
tasktab = load_tasktab()

# bma is the BenchmarkAggr for use with mlr3benchmark
bma_harrell_c = readRDS(fs::path(result_path, "bma_harrell_c.rds"))
bma_isbs = readRDS(fs::path(result_path, "bma_isbs.rds"))

aggr_scores = readRDS(fs::path(result_path, "aggr.rds"))
scores = readRDS(fs::path(result_path, "scores.rds"))

# Create the scaled version of aggregated scores where KM is 0 and best model is 1
aggr_scores_scaled = rescale_aggr_scores(aggr_scores, msr_tbl)

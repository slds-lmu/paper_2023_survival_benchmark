knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE, dev = "ragg_png")

source(here::here("../", "R", "helpers.R"))
source(here::here("../", "R", "plotting.R"))

result_path = here::here("..", "results", "production")

###################################################################################################
### Packages
###################################################################################################
library(mlr3benchmark)
requireNamespace("mlr3extralearners")
library(ggplot2)
library(kableExtra)
library(dplyr)

msr_tbl = load_msr_table(path = here::here("..", "tables", "measures.csv"))

lrntab = load_lrntab(path = here::here("..", "tables", "learners.csv"))
tasktab = load_tasktab(path = here::here("..", "tables", "tasktab.csv"))

# bma is the BenchmarkAggr for use with mlr3benchmark
bma_harrell_c = readRDS(fs::path(result_path, "bma_harrell_c.rds"))
bma_isbs = readRDS(fs::path(result_path, "bma_isbs.rds"))

aggr_scores = readRDS(fs::path(result_path, "aggr.rds"))
aggr_scores[, learner_id := factor(learner_id, lrntab$id)]
scores = readRDS(fs::path(result_path, "scores.rds"))
scores[, learner_id := factor(learner_id, lrntab$id)]

# Create the scaled version of aggregated scores where KM is 0 and best model is 1
aggr_scores_scaled = rescale_aggr_scores(aggr_scores, msr_tbl)

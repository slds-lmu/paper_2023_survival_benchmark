# Plackett-Luce model for the full learner ranking
#
# One PL model fit on all learners (excluding KM and NEL baselines).
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).
#
# Then:
# Plackett-Luce models stratified by task characteristics
#
# Fits separate PL models per subgroup for:
#   1) PH assumption violation (violated vs. not violated)
#   2) Censoring proportion (low vs. high, split at median)
#   3) n/p ratio (low vs. high, split at median)
#
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).

library(PlackettLuce)
library(data.table)
library(ggplot2)
source("R/plackettluce.R")
set.seed(2026)

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper", "PL")

# -- Data -------------------------------------------------------------------
msr_tbl <- load_msr_table()
tasktab <- load_tasktab()
scores_all <- readRDS(fs::path(result_path, "scores.rds"))

# These are always ranked last for harrell_c -> causes issues, just eats DoF in model, omit for more power
exclude <- c("KM", "NEL")

# -- Run unified "full" model --------------------------------------------------
pl_full_harrell_c <- run_pl_ranking(scores_all, "harrell_c", minimize = FALSE, exclude, plot_path, msr_tbl = msr_tbl)
pl_full_isbs <- run_pl_ranking(scores_all, "isbs", minimize = TRUE, exclude, plot_path, msr_tbl = msr_tbl)

# -- PH subgroups -----------------------------------------------------------
pl_ph_hc <- run_pl_ph_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)
pl_ph_isbs <- run_pl_ph_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)

lr_ph_hc <- pl_lr_test(pl_full_harrell_c$pl_fit, pl_ph_hc)
lr_ph_isbs <- pl_lr_test(pl_full_isbs$pl_fit, pl_ph_isbs)

# -- Censoring proportion subgroups -----------------------------------------
res_cens_hc <- run_pl_censprop_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)
res_cens_isbs <- run_pl_censprop_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)

lr_cens_hc <- pl_lr_test(pl_full_harrell_c$pl_fit, res_cens_hc)
lr_cens_isbs <- pl_lr_test(pl_full_isbs$pl_fit, res_cens_isbs)

# -- n/p ratio subgroups ---------------------------------------------------
pl_noverp_hc <- run_pl_noverp_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)
pl_noverp_isbs <- run_pl_noverp_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab, plot_path, msr_tbl = msr_tbl)

lr_noverp_hc <- pl_lr_test(pl_full_harrell_c$pl_fit, pl_noverp_hc)
lr_noverp_isbs <- pl_lr_test(pl_full_isbs$pl_fit, pl_noverp_isbs)

# -- LR test results -----------------------------------------

cli::cli_h1("Likelihood ratio tests")

cli::cli_h2("Based on violation of the PH assumption ({.fun cox.zph} p.value > 0.05)")
cli::cli_inform(c(
  "LR test p-value using {.val harrell_c}: {format.pval(lr_ph_hc$p_value)}",
  "LR test p-value {.val isbs}: {format.pval(lr_ph_isbs$p_value)}"
))

cli::cli_h2("Based on censoring proportion ({.code censprop > median(censprop)})")
cli::cli_inform(c(
  "LR test p-value using {.val harrell_c}: {format.pval(lr_cens_hc$p_value)}",
  "LR test p-value {.val isbs}: {format.pval(lr_cens_isbs$p_value)}"
))

cli::cli_h2("Based on n/p ratio {.code n/p > median(n/p)}")
cli::cli_inform(c(
  "LR test p-value using {.val harrell_c}: {format.pval(lr_noverp_hc$p_value)}",
  "LR test p-value {.val isbs}: {format.pval(lr_noverp_isbs$p_value)}"
))

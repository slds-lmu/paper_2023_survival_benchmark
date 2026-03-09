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
#
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).

library(PlackettLuce)
library(mlr3proba)
library(data.table)
library(ggplot2)
# library(cli)

source("R/plackettluce.R")

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

# These are always ranked last for harrell_c -> causes issues, just eats DoF in model, omit for more power
exclude <- c("KM", "NEL")

# -- Run unified "full" model --------------------------------------------------
res_harrell_c <- run_pl_ranking(scores_all, "harrell_c", minimize = FALSE, exclude, result_path)
res_isbs <- run_pl_ranking(scores_all, "isbs", minimize = TRUE, exclude, result_path)

loglik_harrell_c <- logLik(res_harrell_c$pl_fit)
loglik_isbs <- logLik(res_isbs$pl_fit)


# -- PH subgroups -----------------------------------------------------------
res_ph_hc <- run_pl_ph_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab)
res_ph_isbs <- run_pl_ph_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab)

# Ad hoc likelihood ratio test
loglik_ph_harrell_c <- logLik(res_ph_hc$pl_fits$`PH not violated`)
loglik_noph_harrell_c <- logLik(res_ph_hc$pl_fits$`PH violated`)

loglik_ph_isbs <- logLik(res_ph_isbs$pl_fits$`PH not violated`)
loglik_noph_isbs <- logLik(res_ph_isbs$pl_fits$`PH violated`)

p_value_ph_harrell_c <- pchisq(
  q = 2 * as.numeric(loglik_ph_harrell_c + loglik_noph_harrell_c - loglik_harrell_c),
  # k-1 free parameters
  df = length(coef(res_harrell_c$pl_fit)),
  lower.tail = FALSE
)

p_value_ph_isbs <- pchisq(
  q = 2 * as.numeric(loglik_ph_isbs + loglik_noph_isbs - loglik_isbs),
  # k-1 free parameters
  df = length(coef(res_harrell_c$pl_fit)),
  lower.tail = FALSE
)


# -- Censoring proportion subgroups -----------------------------------------
res_cens_hc <- run_pl_censprop_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab)
res_cens_isbs <- run_pl_censprop_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab)


loglik_censlow_harrell_c <- logLik(res_cens_hc$pl_fits$`Low censoring (<= 0.45)`)
loglik_censhigh_harrell_c <- logLik(res_cens_hc$pl_fits$`High censoring (> 0.45)`)

loglik_censlow_isbs <- logLik(res_cens_isbs$pl_fits$`Low censoring (<= 0.45)`)
loglik_censhigh_isbs <- logLik(res_cens_isbs$pl_fits$`High censoring (> 0.45)`)

p_value_cens_harrell_c <- pchisq(
  q = 2 * as.numeric(loglik_censlow_harrell_c + loglik_censhigh_harrell_c - loglik_harrell_c),
  # k-1 free parameters
  df = length(coef(res_harrell_c$pl_fit)),
  lower.tail = FALSE
)

p_value_cens_isbs <- pchisq(
  q = 2 * as.numeric(loglik_censlow_isbs + loglik_censhigh_isbs - loglik_isbs),
  # k-1 free parameters
  df = length(coef(res_harrell_c$pl_fit)),
  lower.tail = FALSE
)


# -- LR test results -----------------------------------------

cli::cli_h1("Likelihood ratio tests")

cli::cli_h2("Based on violation of the PH assumption")

cli::cli_inform(c(
  "LR test p-value using {.val harrell_c}: {format.pval(p_value_ph_harrell_c)}",
  "LR test p-value {.val isbs}: {format.pval(p_value_ph_isbs)}"
))
cli::cli_h2("Based on censoring proportion")

cli::cli_inform(c(
  "LR test p-value using {.val harrell_c}: {format.pval(p_value_cens_harrell_c)}",
  "LR test p-value {.val isbs}: {format.pval(p_value_cens_isbs)}"
))

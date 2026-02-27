library(PlackettLuce)

tasktab = load_tasktab()

tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]

bma_harrell_c = readRDS(fs::path(conf$result_path, "bma_harrell_c.rds"))
ranks_harrell_c = bma_harrell_c$rank_data("harrell_c", minimize = FALSE)

ranks_ph_ok = ranks_harrell_c[, tasktab[ph_violated == 0, task_id]]
ranks_ph_nok = ranks_harrell_c[, tasktab[ph_violated == 1, task_id]]

# 1. Create rankings (transpose so datasets are rows)
rankings_ph_ok = as.rankings(t(ranks_ph_ok))
rankings_ph_nok = as.rankings(t(ranks_ph_nok))

length(rankings_ph_ok)
length(rankings_ph_nok)


# Fit the model for ph ook
tictoc::tic("PL PH ok")
pl_fit_ph_ok <- PlackettLuce(rankings_ph_ok, trace = TRUE, maxit = 400)
tictoc::toc()
# View results
summary(pl_fit_ph_ok)


# Fit the model for ph nok
tictoc::tic("PL PH not ok")
pl_fit_ph_nok <- PlackettLuce(rankings_ph_nok, trace = TRUE, maxit = 400)
tictoc::toc()


# View results
summary(pl_fit_ph_ok)

# Worth parameters (probabilities, sum to 1)
coef(pl_fit_ph_ok, log = FALSE)

qv_ph_ok <- qvcalc(pl_fit)
qv_ph_nok <- qvcalc(pl_fit_ph_nok)


ragg::agg_png(filename = "pl-worth-ph-ok.png")
plot(qv_ph_ok, ylab = "Worth (log)", main = NULL)
dev.off()

ragg::agg_png(filename = "pl-worth-ph-nok.png")
plot(qv_ph_nok, ylab = "Worth (log)", main = NULL)
dev.off()

library(PlackettLuce)
conf = config::get(config = "production")

tasktab = load_tasktab()

tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]
tasktab[, noverp := n / p]

bma_harrell_c = readRDS(fs::path(conf$result_path, "bma_harrell_c.rds"))
ranks_harrell_c = bma_harrell_c$rank_data("harrell_c", minimize = FALSE)

# Exclude KM and NEL as they always place last anway, just make it harder to model
ranks_harrell_c = ranks_harrell_c[!(rownames(ranks_harrell_c) %in% c("KM", "NEL")), ]


# Subset to only very different learners for debugging PL
# ranks_harrell_c = ranks_harrell_c[(rownames(ranks_harrell_c) %in% c("MBSTAFT", "Pen", "RRT")), ]

# 1. Create rankings (transpose so datasets are rows)
rankings = as.rankings(t(ranks_harrell_c))

# sanity check and order
tasktab = tasktab[match(dimnames(rankings)[[1]], tasktab$task_id)]
all.equal(tasktab$task_id, dimnames(rankings)[[1]])


# 2. Group rankings

# each dataset is its own group -> did not fit in the end
G = group(rankings, index = seq_len(nrow(t(ranks_harrell_c))))

# Create groups: 1 -> PH not violated, 2 -> PH violated
# creates two groups of rankings, other covars need to be separated into 2 groups as well?
# G = group(rankings, index = 1 + as.integer(tasktab$ph_violated))

# 3. Create data frame (with rankings included so we can subset model_data for testing on fewer rankings, but if G has fewer grous)
model_data <- data.frame(
  G = G,
  censprop = tasktab$censprop,
  censcut = cut(
    tasktab$censprop,
    breaks = 2,
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
  ),
  ph_violated = tasktab$ph_violated
)

# 4. Fit the tree
tictoc::tic()
pltreefit = pltree(
  formula = G ~ ph_violated,
  data = model_data,
  # Normally no pseudo rankings needed as all learners are ranked on all tasks,
  # but when subgrouping by covariates connectivity may be affected, so we set the default of 0.5
  npseudo = 0.5,
  gamma = TRUE,
  # Control args passed to partykit via ..., not directly documented in ?pltree, didnt do anything useful
  # control = partykit::mob_control(
  #   cores = 20,
  #   verbose = TRUE
  # ),
  minsize = 5, # minimum observations per node
  maxdepth = 3, # limit tree depth
  epsilon = 1e-6, # less strict convergence for PlackettLuce
  # maxit = 300,  # fewer iterations
  trace = TRUE
)
tictoc::toc()
saveRDS(pltreefit, "pltreefit.rds")
pltreefit = readRDS("pltreefit.rds")

summary(pltreefit)
# summary(pltreefit, ref = "CPH")
# summary(pltreefit, ref = 3L)

worth <- coef(pltreefit, log = FALSE)
sort(worth[!grepl("tie", names(worth))], decreasing = TRUE)

ragg::agg_png(filename = "pltree-censprop-ph.png", width = 800, height = 700)
plot(pltreefit, names = FALSE, abbreviate = 2, ylines = 2)
dev.off()

# library(igraph)
# A <- adjacency(rankings)
# net <- graph_from_adjacency_matrix(A)
# plot(net, edge.arrow.size = 0.5, vertex.size = 30)

if (FALSE) {
  library(PlackettLuce)
  bma_harrell_c = readRDS(fs::path(conf$result_path, "bma_harrell_c.rds"))
  ranks_harrell_c = bma_harrell_c$rank_data("harrell_c", minimize = FALSE)

  # Create rankings (transpose so datasets are rows, algorithms are columns)
  rankings <- as.rankings(t(ranks_harrell_c))

  # Fit the model
  tictoc::tic()
  pl_fit <- PlackettLuce(rankings, trace = TRUE, maxit = 100)
  tictoc::toc()
  # View results
  summary(pl_fit)

  # Worth parameters (probabilities, sum to 1)
  coef(pl_fit, log = FALSE)

  qv <- qvcalc(pl_fit)
  ragg::agg_png(filename = "pl-worth.png")
  plot(qv, ylab = "Worth (log)", main = NULL)
  dev.off()
  ragg::agg_png(filename = "pl-worth-cphref.png")
  plot(qv, ref = "CPH", ylab = "Worth (log)", main = NULL)
  dev.off()
}

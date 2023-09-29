# Par / CarpenterFdaData / rcll -------------------------------------------
# https://github.com/RaphaelS1/proba_benchmark/issues/47

set.seed(123) # same as in settings.R

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")

CarpenterFdaData = as_task_surv(readRDS(here::here("code/data/CarpenterFdaData.rds")), target = "time", event = "status", id = "CarpenterFdaData")
CarpenterFdaData$set_col_roles("status", add_to = "stratum")

# auto_tune relies on global variable `measure` for tuning measure
measure = msr("surv.rcll", id = "rcll")

bl = function(key, ..., .encode = FALSE, .scale = FALSE) { # get base learner with fallback + encapsulation
  learner = lrn(key, ...)
  #fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)

  # As per RS to fix #38
  #fallback$predict_type = "crank"
  learner$predict_type = "crank"

  #learner$fallback = fallback
  #learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  # Added form as per RS
  g = ppl("distrcompositor", learner = learner, form = 'ph')

  if (.scale) {
    g = po("scale") %>>% g
  }

  if (.encode) {
    g = po("encode", method = "treatment") %>>% g
  }

  as_learner(po("fixfactors") %>>% po("collapsefactors", target_level_count = 5) %>>% g)
}

auto_tune = function(learner, ...) { # wrap into random search
  learner = as_learner(learner)
  search_space = ps(...)
  if (is.null(search_space$trafo))
    checkmate::assert_subset(names(search_space$params), names(learner$param_set$params))

  at = AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("cv", folds = 3),
    measure = msr("surv.rcll", id = "rcll"),
    terminator = trm("evals", n_evals = 3, k = 0),
    tuner = tnr("random_search"),
    store_tuning_instance = TRUE,
    store_benchmark_result = FALSE,
    store_models = TRUE # Preserve models for debugging
  )
}

Par = auto_tune(
  bl("surv.parametric", type = "aft"),
  surv.parametric.dist = p_fct(c("weibull", "lognormal", "loglogistic"))
)

rsmp_cfd = c(1L, 4L, 12L, 16L, 38L, 39L, 40L, 41L, 51L, 52L, 59L, 66L, 70L,
             76L, 79L, 81L, 83L, 95L, 99L, 100L, 104L, 109L, 118L, 125L, 126L,
             127L, 132L, 133L, 136L, 147L, 152L, 155L, 159L, 162L, 163L, 176L,
             183L, 186L, 201L, 204L, 212L, 220L, 221L, 233L, 235L, 240L, 250L,
             269L, 272L, 273L, 274L, 277L, 326L, 5L, 8L, 9L, 10L, 13L, 18L,
             19L, 32L, 50L, 60L, 72L, 85L, 86L, 89L, 97L, 113L, 115L, 122L,
             144L, 156L, 160L, 164L, 170L, 171L, 175L, 177L, 180L, 189L, 206L,
             209L, 210L, 211L, 225L, 226L, 232L, 237L, 239L, 246L, 249L, 253L,
             256L, 260L, 264L, 265L, 266L, 275L, 276L, 279L, 283L, 285L, 303L,
             310L, 408L, 6L, 15L, 20L, 24L, 25L, 27L, 30L, 31L, 34L, 37L,
             42L, 44L, 46L, 47L, 55L, 71L, 74L, 75L, 77L, 78L, 82L, 90L, 92L,
             114L, 117L, 139L, 143L, 148L, 150L, 166L, 167L, 181L, 182L, 192L,
             196L, 200L, 203L, 207L, 216L, 223L, 241L, 242L, 244L, 248L, 254L,
             255L, 261L, 268L, 270L, 281L, 282L, 307L, 3L, 7L, 17L, 22L, 28L,
             35L, 43L, 49L, 53L, 58L, 63L, 65L, 67L, 73L, 87L, 93L, 102L,
             103L, 106L, 123L, 129L, 131L, 134L, 137L, 138L, 141L, 142L, 146L,
             151L, 153L, 157L, 158L, 161L, 165L, 179L, 188L, 190L, 191L, 194L,
             197L, 219L, 227L, 228L, 234L, 236L, 238L, 252L, 257L, 259L, 263L,
             267L, 308L, 11L, 33L, 56L, 98L, 101L, 119L, 121L, 169L, 174L,
             195L, 231L, 278L, 284L, 316L, 327L, 329L, 337L, 352L, 353L, 357L,
             363L, 366L, 369L, 374L, 382L, 385L, 386L, 390L, 397L, 398L, 105L,
             168L, 218L, 251L, 290L, 301L, 304L, 305L, 306L, 317L, 319L, 321L,
             325L, 330L, 336L, 350L, 351L, 354L, 367L, 370L, 377L, 378L, 383L,
             389L, 391L, 392L, 396L, 404L, 405L, 84L, 96L, 185L, 202L, 205L,
             292L, 293L, 294L, 311L, 315L, 322L, 334L, 335L, 339L, 341L, 343L,
             344L, 349L, 355L, 356L, 359L, 360L, 361L, 375L, 379L, 384L, 387L,
             402L, 406L, 112L, 120L, 124L, 217L, 222L, 286L, 291L, 296L, 299L,
             302L, 309L, 312L, 320L, 324L, 331L, 332L, 333L, 338L, 340L, 346L,
             348L, 362L, 364L, 371L, 372L, 376L, 380L, 395L, 399L)

Par$train(CarpenterFdaData, row_ids = rsmp_cfd)


# Trying the manual way ---------------------------------------------------

set.seed(123) # same as in settings.R

library("mlr3misc")
library("mlr3")
library("mlr3proba")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3tuning")
requireNamespace("mlr3extralearners")

CarpenterFdaData = as_task_surv(readRDS(here::here("code/data/CarpenterFdaData.rds")), target = "time", event = "status", id = "CarpenterFdaData")
CarpenterFdaData$set_col_roles("status", add_to = "stratum")

dbglrn <- lrn("surv.parametric", type = "aft", dist = "loglogistic")
#dbglrn$train(CarpenterFdaData)
# dbglrn$predict(CarpenterFdaData)$score(msr("surv.rcll"))

set.seed(123)
rr <- resample(
  CarpenterFdaData,
  lrn("surv.parametric", type = "aft", dist = "loglogistic"),
  rsmp("cv", folds = 9)
)

rr$score(msr("surv.rcll"))

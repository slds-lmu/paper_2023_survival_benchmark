get_learners = function(folds = 3, n_evals = 100) {

  fallback = ppl("crankcompositor", lrn("surv.kaplan"), response = TRUE, method = "mean",
                 overwrite = FALSE, graph_learner = TRUE)

  learns = list()

  learns$kaplan = lrn("surv.kaplan", id = "class_nonpar_kaplan")
  learns$kaplan$encapsulate = c(train = "evaluate", predict = "evaluate")
  learns$kaplan$fallback = fallback

  learns$akritas = lrn("surv.akritas", id = "class_nonpar_akritas")
  learns$akritas$encapsulate = c(train = "evaluate", predict = "evaluate")
  learns$akritas$fallback = fallback

  learns$cox = lrn("surv.coxph", id = "class_semipar_coxph")
  learns$cox$encapsulate = c(train = "evaluate", predict = "evaluate")
  learns$cox$fallback = fallback

  learner = lrn("surv.cv_glmnet", id = "class_semipar_cvglmnet")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$glm = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
                    mlr3misc::invoke(
                      AutoTuner$new,
                      learner = learner,
                      search_space = ParamSet$new(list(ParamDbl$new("alpha", lower = 0, upper = 1))),
                      resampling = rsmp("cv", folds = folds),
                      measure = msr("surv.cindex"),
                      terminator = trm("evals", n_evals = n_evals),
                      tuner = tnr("random_search"))))$
      add_edge("encode", "nop")$
      add_edge("nop", "class_semipar_cvglmnet.tuned"))

  learner = lrn("surv.penalized", id = "class_semipar_penalized")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$penalized = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ParamSet$new(list(
      ParamDbl$new("lambda1", lower = 0, upper = 10),
      ParamDbl$new("lambda2", lower = 0, upper = 10)
    )),
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  learner = lrn("surv.parametric", id = "class_par_param", type = "aft")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$parametric = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ParamSet$new(list(
      ParamFct$new("dist", levels = c("weibull", "logistic", "lognormal", "loglogistic"))
    )),
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("grid_search"))

  learner = lrn("surv.flexible", id = "class_par_flex")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$flexible = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ParamSet$new(list(
      ParamInt$new("k", lower = 1, upper = 7)
    )),
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("grid_search"))

  ps = ParamSet$new(list(
    ParamInt$new("ntree", lower = 250, upper = 2500),
    ParamInt$new("mtry", lower = 1, upper = 12),
    ParamInt$new("nodesize", lower = 1, upper = 20)
  ))

  options(rf.cores = 1)
  learner = lrn("surv.rfsrc", id = "ml_ranfor_rfsrc_brier", splitrule = "bs.gradient")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$rfsrc_brier = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  learner = lrn("surv.rfsrc", id = "ml_ranfor_rfsrc_logrank", splitrule = "logrank")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$rfsrc_logrank = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  ps = ParamSet$new(list(
    ParamInt$new("num.trees", lower = 250, upper = 2500),
    ParamInt$new("min.node.size", lower = 1, upper = 20),
    ParamInt$new("mtry", lower = 1, upper = 12)
  ))

  learner = lrn("surv.ranger", id = "ml_ranfor_ranger_c", splitrule = "C")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$ranger_c = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  ps = ParamSet$new(list(
    ParamInt$new("ntree", lower = 250, upper = 2500),
    ParamInt$new("mtry", lower = 1, upper = 12)
  ))

  learner = lrn("surv.cforest", id = "ml_ranfor_rscif")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$rscif = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  ps = ParamSet$new(list(
    ParamInt$new("minbucket", lower = 1, upper = 20),
    ParamInt$new("maxdepth", lower = 2, upper = 30)
  ))

  learner = lrn("surv.rpart", id = "ml_ranfor_rrt")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$rpart = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))


  ps = ParamSet$new(list(
    ParamInt$new("mstop", lower = 10, upper = 2500),
    ParamDbl$new("nu", lower = 0, upper = 0.1),
    ParamFct$new("baselearner", levels = c("bols", "btree"))
  ))

  learner = lrn("surv.mboost", id = "ml_gbm_mboost_coxph", family = "coxph")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$mboost_coxph = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  learner = lrn("surv.mboost", id = "ml_gbm_mboost_cindex", family = "cindex")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$mboost_cindex = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  learner = lrn("surv.mboost", id = "ml_gbm_mboost_gehan", family = "gehan")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$mboost_gehan = mlr3misc::invoke(
    AutoTuner$new,
    learner = learner,
    search_space = ps,
    resampling = rsmp("cv", folds = folds),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = n_evals),
    tuner = tnr("random_search"))

  learns$coxboost = lrn("surv.cv_coxboost", id = "ml_gbm_coxboost",
                  penalty = "optimCoxBoostPenalty", maxstepno = 1000)
  learns$coxboost$encapsulate = c(train = "evaluate", predict = "evaluate")
  learns$coxboost$fallback = fallback

  ps = ParamSet$new(list(
    ParamFct$new("kernel", levels = c("lin_kernel", "rbf_kernel")),
    ParamDbl$new("gamma", lower = 1e-3, upper = 1e3),
    ParamDbl$new("mu", lower = 1e-3, upper = 1e3)
  ))
  ps$trafo = function(x, param_set) {
    x$gamma.mu = c(x$gamma, x$mu)
    x$gamma = x$mu = NULL
    return(x)
  }

  learner = lrn("surv.svm", id = "ml_svm_van", type = "hybrid", gamma.mu = 0,
                diff.meth = "makediff3")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$ssvm = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
        mlr3misc::invoke(
          AutoTuner$new,
          learner = learner,
          search_space = ps,
          resampling = rsmp("cv", folds = folds),
          measure = msr("surv.cindex"),
          terminator = trm("evals", n_evals = n_evals),
          tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_svm_van.tuned"))

  ps = ParamSet$new(list(
    ParamDbl$new("dropout", lower = 0, upper = 1),
    ParamDbl$new("weight_decay", lower = 0, upper = 0.5),
    ParamDbl$new("learning_rate", lower = 0, upper = 1),
    ParamInt$new("nodes", lower = 1, upper = 32),
    ParamInt$new("k", lower = 1, upper = 4)
  ))
  ps$trafo = function(x, param_set) {
    x$num_nodes = rep(x$nodes, x$k)
    x$nodes = x$k = NULL
    return(x)
  }

  learner = lrn("surv.coxtime", id = "ml_ann_coxtime", frac = 0.3, standardize_time = TRUE,
                num_nodes = 0, optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$coxtime = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
        mlr3misc::invoke(
          AutoTuner$new,
          learner = learner,
          search_space = ps,
          resampling = rsmp("cv", folds = folds),
          measure = msr("surv.cindex"),
          terminator = trm("evals", n_evals = n_evals),
          tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_coxtime.tuned"))

  learner = lrn("surv.deephit", id = "ml_ann_deephit", frac = 0.3, num_nodes = 0,
                optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$deephit = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
        mlr3misc::invoke(
          AutoTuner$new,
          learner = learner,
          search_space = ps,
          resampling = rsmp("cv", folds = folds),
          measure = msr("surv.cindex"),
          terminator = trm("evals", n_evals = n_evals),
          tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_deephit.tuned"))

  learner = lrn("surv.deepsurv", id = "ml_ann_deepsurv", frac = 0.3,
                num_nodes = 0, optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$deepsurv = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
                    mlr3misc::invoke(
                      AutoTuner$new,
                      learner = learner,
                      search_space = ps,
                      resampling = rsmp("cv", folds = folds),
                      measure = msr("surv.cindex"),
                      terminator = trm("evals", n_evals = n_evals),
                      tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_deepsurv.tuned"))

  learner = lrn("surv.loghaz", id = "ml_ann_loghaz", frac = 0.3, num_nodes = 0,
                optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$nnetsurv = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
                    mlr3misc::invoke(
                      AutoTuner$new,
                      learner = learner,
                      search_space = ps,
                      resampling = rsmp("cv", folds = folds),
                      measure = msr("surv.cindex"),
                      terminator = trm("evals", n_evals = n_evals),
                      tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_loghaz.tuned"))

  learner = lrn("surv.pchazard", id = "ml_ann_pchaz", frac = 0.3, num_nodes = 0,
                optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$pchaz = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
                    mlr3misc::invoke(
                      AutoTuner$new,
                      learner = learner,
                      search_space = ps,
                      resampling = rsmp("cv", folds = folds),
                      measure = msr("surv.cindex"),
                      terminator = trm("evals", n_evals = n_evals),
                      tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_pchaz.tuned"))

  ps = ParamSet$new(list(
    ParamDbl$new("decay", lower = 0, upper = 0.5),
    ParamDbl$new("lr", lower = 0, upper = 1)
  ))

  learner = lrn("surv.dnnsurv", id = "ml_ann_dnn", validation_split = 0.3,
                optimizer = "adam", early_stopping = TRUE, epochs = 100)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner$fallback = fallback
  learns$dnnsurv = GraphLearner$new(
    Graph$new()$
      add_pipeop(po("scale"))$
      add_pipeop(po("encode", method = "treatment"))$
      add_pipeop(po("nop"))$
      add_pipeop(po("learner",
                    mlr3misc::invoke(
                      AutoTuner$new,
                      learner = learner,
                      search_space = ps,
                      resampling = rsmp("cv", folds = folds),
                      measure = msr("surv.cindex"),
                      terminator = trm("evals", n_evals = n_evals),
                      tuner = tnr("random_search"))))$
      add_edge("scale", "encode")$
      add_edge("encode", "nop")$
      add_edge("nop", "ml_ann_dnn.tuned"))

  learns = lapply(learns, function(x) {
    x = ppl("crankcompositor",
            ppl("distrcompositor", x, form = "ph", estimator = "kaplan", overwrite = FALSE),
            response = TRUE, method = "mean", overwrite = FALSE, graph_learner = TRUE)
    return(x)
  })

  return(learns)
}

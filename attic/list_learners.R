lrnlist <- list(
    KM = list(learner = "surv.kaplan")
    ,
    NL = list(learner = "surv.nelson")
    ,
    AF = list(learner = "surv.akritas")
    ,
    CPH = list(learner = "surv.coxph")
    ,
    GLM = list(learner = "surv.cv_glmnet", .encode = TRUE)
    ,
    Pen = list(learner = "surv.penalized")
    ,
    Par = list(learner = "surv.parametric")
    ,
    Flex = list(learner = "surv.flexible")
    ,
    RFSRC = list(learner = "surv.rfsrc")
    ,
    RAN = list(learner = "surv.ranger")
    ,
    CIF = list(learner = "surv.cforest")
    ,
    ORSF = list(learner = "surv.aorsf")
    ,
    RRT = list(learner = "surv.rpart")
    ,
    MBO = list(learner = "surv.mboost")
    ,
    CoxB = list(learner = "surv.cv_coxboost", .encode = TRUE)
    ,
    XGB = list(learner = "surv.xgboost", .encode = TRUE)
    ,
    SSVM = list(learner = "surv.svm", .encode = TRUE, .scale = TRUE)
    # ,
    # CoxT = list(learner = "surv.coxtime", .encode = TRUE, .scale = TRUE)
    # ,
    # DH = list(learner = "surv.deephit", .encode = TRUE, .scale = TRUE)
    # ,
    # DS = list(learner = "surv.deepsurv", .encode = TRUE, .scale = TRUE)
    # ,
    # LH = list(learner = "surv.loghaz", .encode = TRUE, .scale = TRUE)
    # ,
    # PCH = list(learner = "surv.pchazard", .encode = TRUE, .scale = TRUE)
    # ,
    # DNN = list(learner = "surv.dnnsurv", .encode = TRUE, .scale = TRUE)
  )

lrnlist |>
  lapply(as.data.frame) |>
  data.table::rbindlist(fill = TRUE) |>
  dplyr::mutate(across(c(".encode", ".scale"), ~ifelse(is.na(.x), "-", "Yes"))) |>
  kableExtra::kbl() |>
  kableExtra::kable_styling(bootstrap_options = "striped")

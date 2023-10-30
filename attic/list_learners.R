lrnlist <- list(
    KM = list(learner = "surv.kaplan", params = 0)
    ,
    NL = list(learner = "surv.nelson", params = 0)
    ,
    AF = list(learner = "surv.akritas", params = 1)
    ,
    CPH = list(learner = "surv.coxph", params = 0)
    ,
    GLM = list(learner = "surv.cv_glmnet", .encode = TRUE, params = 1, internal_cv = TRUE)
    ,
    Pen = list(learner = "surv.penalized", params = 2)
    ,
    Par = list(learner = "surv.parametric", params = 1)
    ,
    Flex = list(learner = "surv.flexible", params = 1)
    ,
    RFSRC = list(learner = "surv.rfsrc", params = 5)
    ,
    RAN = list(learner = "surv.ranger", params = 5)
    ,
    CIF = list(learner = "surv.cforest", params = 5)
    ,
    ORSF = list(learner = "surv.aorsf", params = 2)
    ,
    RRT = list(learner = "surv.rpart", params = 1)
    ,
    MBO = list(learner = "surv.mboost", params = 4)
    ,
    CoxB = list(learner = "surv.cv_coxboost", .encode = TRUE, params = 0, internal_cv = TRUE)
    ,
    XGB = list(learner = "surv.xgboost", .encode = TRUE, params = 6)
    ,
    SSVM = list(learner = "surv.svm", .encode = TRUE, .scale = TRUE, params = 4)
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
  ) |>
  lapply(as.data.frame) |>
  data.table::rbindlist(fill = TRUE, idcol = TRUE) |>
  dplyr::mutate(across(where(is.logical), ~ifelse(is.na(.x), "-", "Yes")))

write.csv(lrnlist, file = here::here("attic/learners.csv"), row.names = FALSE)

lrnlist |>
  kableExtra::kbl() |>
  kableExtra::kable_styling(bootstrap_options = "striped")

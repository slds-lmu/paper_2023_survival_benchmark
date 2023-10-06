# Deep learner auto-tuner setup saved for posterity

list(
   CoxT = auto_tune(
     bl("surv.coxtime", standardize_time = TRUE, epochs = 100, optimizer = "adam", .encode = TRUE, .scale = TRUE),
     surv.coxtime.dropout = p_dbl(0, 1),
     surv.coxtime.weight_decay = p_dbl(0, 0.5),
     surv.coxtime.learning_rate = p_dbl(0, 1),
     surv.coxtime.nodes = p_int(1, 32),
     surv.coxtime.k = p_int(1, 4),
     .extra_trafo = function(x, param_set) {
       x$surv.coxtime.num_nodes = rep(x$surv.coxtime.nodes, x$surv.coxtime.k)
       x$surv.coxtime.nodes = x$surv.coxtime.k = NULL
       x
     }
   )

   ,

    DH = auto_tune(
      bl("surv.deephit", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.deephit.nodes = p_int(1, 32),
      surv.deephit.k = p_int(1, 4),
      surv.deephit.dropout = p_dbl(0, 1),
      surv.deephit.weight_decay = p_dbl(0, 5),
      surv.deephit.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.deephit.num_nodes = rep(x$surv.deephit.nodes, x$surv.deephit.k)
        x$surv.deephit.nodes = x$surv.deephit.k = NULL
        x
      }
    )

    ,

    DS = auto_tune(
      bl("surv.deepsurv", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.deepsurv.nodes = p_int(1, 32),
      surv.deepsurv.k = p_int(1, 4),
      surv.deepsurv.dropout = p_dbl(0, 1),
      surv.deepsurv.weight_decay = p_dbl(0, 5),
      surv.deepsurv.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.deepsurv.num_nodes = rep(x$surv.deepsurv.nodes, x$surv.deepsurv.k)
        x$surv.deepsurv.nodes = x$surv.deepsurv.k = NULL
        x
      }
    )

    ,

    LH = auto_tune(
      bl("surv.loghaz", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.loghaz.nodes = p_int(1, 32),
      surv.loghaz.k = p_int(1, 4),
      surv.loghaz.dropout = p_dbl(0, 1),
      surv.loghaz.weight_decay = p_dbl(0, 5),
      surv.loghaz.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.loghaz.num_nodes = rep(x$surv.loghaz.nodes, x$surv.loghaz.k)
        x$surv.loghaz.nodes = x$surv.loghaz.k = NULL
        x
      }
    )

    ,

    PCH = auto_tune(
      bl("surv.pchazard", optimizer = "adam", .encode = TRUE, .scale = TRUE),
      surv.pchazard.nodes = p_int(1, 32),
      surv.pchazard.k = p_int(1, 4),
      surv.pchazard.dropout = p_dbl(0, 1),
      surv.pchazard.weight_decay = p_dbl(0, 5),
      surv.pchazard.learning_rate = p_dbl(0, 1),
      .extra_trafo = function(x, param_set) {
        x$surv.pchazard.num_nodes = rep(x$surv.pchazard.nodes, x$surv.pchazard.k)
        x$surv.pchazard.nodes = x$surv.pchazard.k = NULL
        x
      }
    )

    ,

    DNN = auto_tune(
      bl("surv.dnnsurv", optimizer = "adam", epochs = 100, .encode = TRUE, .scale = TRUE),
      surv.dnnsurv.decay = p_dbl(0, 0.5),
      surv.dnnsurv.lr = p_dbl(0, 1),
      surv.dnnsurv.cuts = p_int(3, 50)
    )
)

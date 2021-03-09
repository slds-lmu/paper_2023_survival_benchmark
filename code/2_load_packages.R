check_pkg_versions <- function(install = FALSE, ...) {
    x <- data.frame(
        pkg = c("survival", "checkmate", "distr6", "mlr3", "mlr3extralearners",
                "mlr3learners", "mlr3misc", "mlr3pipelines", "mlr3proba",
                "mlr3tuning", "paradox", "reticulate", "future"),
        version = c("3.2.3", "2.0.0", "1.4.7", "0.9.0", "0.3.0", "0.5.0",
                    "0.5.0", "0.3.0", "0.3.0", "0.2.0", "0.6.0", "1.16",
                    "1.20.1")
    )

    which <- vapply(seq(nrow(x)), function(i) utils::packageVersion(x[i, 1]) < x[i, 2],
            logical(1))

    if (any(which)) {
        pkgs <- x$pkg[which]
        if (install) {
            if ("mlr3extralearners" %in% pkgs) {
                remotes::install_github("mlr-org/mlr3extralearners",
                                         upgrade = "never", ...)
                pkgs <- pkgs[!(pkgs %in% "mlr3extralearners")]
            }

            install.packages(pkgs, ...)
        } else {
            stop(sprintf("The following packages need updating: %s",
                paste0("{", paste0(pkgs, collapse = ", "), "}")))
        }
    }
}
check_pkg_versions(TRUE)

library(survival)
library(checkmate)
library(tidyverse)
library(dplyr)
library(magrittr)
library(distr6)
library(mlr3)
library(mlr3extralearners)
library(mlr3learners)
library(mlr3misc)
library(mlr3pipelines)
library(mlr3proba)
library(mlr3tuning)
library(paradox)
library(reticulate)
library(future)
library(future.apply)

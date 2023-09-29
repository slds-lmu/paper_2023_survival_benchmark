source(here::here("attic/batchmark-mini.R"), echo = FALSE)


tbl <- data.table::rbindlist(lapply(tasks, function(t) {
  n <- t$nrow
  tsmall <- nrow(t$data(col = "time")[time < 1, ])

  data.frame(task = t$id, n = n, tsmall = tsmall, p = round(tsmall/n, 3))
}))

tbl <- data.table::setorder(tbl[tsmall > 0], "p")
tbl |>
  knitr::kable()

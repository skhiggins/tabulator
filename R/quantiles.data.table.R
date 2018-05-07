quantiles.data.table <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  vars <- dplyr::quos(...) %>% quo_to_chr()
  tabbed <- df[, lapply(.SD, function(x) quantile(x, probs = probs, na.rm = na.rm)),
    .SDcols = vars
  ][, p := probs] %>% setcolorder(c("p", vars))
  tabbed[] # make sure it prints
}

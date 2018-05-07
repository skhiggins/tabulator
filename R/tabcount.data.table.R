tabcount.data.table <- function(df, ...) {
  group_by <- dplyr::quos(...) %>% quo_to_chr()
  df[, .N, by = group_by][, .N]
}

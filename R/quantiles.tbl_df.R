quantiles.tbl_df <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  vars <- dplyr::quos(...)
  df %>%
    dplyr::summarize(p = list(probs), q = list(quantile(!!!vars, probs))) %>%
    tidyr::unnest()
}

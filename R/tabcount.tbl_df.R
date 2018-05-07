tabcount.tbl_df <- function(df, ...) {
  group_by <- dplyr::quos(...)
  df %>%
    dplyr::n_distinct(!!!group_by) # !!! since it's a quosure
}

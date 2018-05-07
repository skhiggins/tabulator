tab.tbl_df <- function(df, ..., round = 2) { # to check without requiring tibble
  group_by <- dplyr::quos(...)
  rowsofdata <- nrow(df)
  df %>%
    dplyr::group_by(!!!group_by) %>% # !!! since it's a quosure
    dplyr::summarize(N = n()) %>%
    dplyr::arrange(desc(N)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      temp_prop = N/rowsofdata,
      prop = round(temp_prop, digits = round),
      cum_prop = round(cumsum(temp_prop), digits = round)
    ) %>%
    dplyr::select(-temp_prop)
}

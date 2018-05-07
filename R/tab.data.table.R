tab.data.table <- function(df, ..., round=2) { # note ... is the variable names to group by
  group_by <- dplyr::quos(...) %>% quo_to_chr()
  rowsofdata <- df[, .N] # faster than nrow() on big data.tables
  df[, .N, by = group_by][,
    temp_prop := N/rowsofdata][,
    prop := round(temp_prop, digits = round)][
    order(-N)][, # sort in descending order by N before cumulative prop
    cum_prop := round(cumsum(temp_prop), digits = round)][,
    temp_prop := NULL][ # remove temp var
    order(-N)] # make sure final data.table sorted
}

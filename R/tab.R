#' Efficient tabulation
#'
#' Produces a tabulation: for each unique group from the variable(s),
#' \code{tab} shows the number of
#' 	observations with that value, proportion of observations with that
#' 		value, and cumulative proportion, in descending order of frequency.
#' 		Accepts data.table or tibble as input.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{tab} uses \code{data.table} syntax.
#'
#' @param df A data.table or tibble
#' @param ... A column or set of columns (without quotation marks)
#' @param round An integer indicating the number of digits for proportion and cumulative proportion
#' @return Tabulation (frequencies, proportion, cumulative proportion) for each unique value of the variables given in \code{...} from \code{df}.
#' @examples
#' # data.table
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' a %>% tab(varname)
#'
#' # tibble
#' b <- tibble(varname = sample.int(20, size = 1000000, replace = TRUE))
#' b %>% tab(varname)
#'
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @export
tab <- function(df, ...) {
  UseMethod("tab", df)
}

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

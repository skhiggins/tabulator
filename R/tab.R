#' Efficient tabulation
#'
#' Produces a tabulation: for each unique group from the variable(s),
#' \code{tab} shows the number of
#' 	observations with that value, proportion of observations with that
#' 		value, and cumulative proportion, in descending order of frequency.
#' 		Accepts data.table, tibble, or data.frame as input.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{tab} uses \code{data.table} syntax.
#'
#' @param df A data.table, tibble, or data.frame
#' @param ... A column or set of columns (without quotation marks)
#' @param round An integer indicating the number of digits for proportion and cumulative proportion
#' @return Tabulation (frequencies, proportion, cumulative proportion) for each unique value of the variables given in \code{...} from \code{df}.
#' @examples
#' # data.table
#' library(data.table)
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' tab(a, varname)
#'
#' # tibble
#' library(tibble)
#' b <- tibble(varname = sample.int(20, size = 1000000, replace = TRUE))
#' tab(b, varname)
#'
#' # data.frame
#' c <- data.frame(varname = sample.int(20, size = 1000000, replace = TRUE))
#' tab(c, varname)
#'
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @export
tab <- function(df, ...) {
  UseMethod("tab", df)
}

#' @export
tab.data.table <- function(df, ..., round=2) { # note ... is the variable names to group by
  dt <- df # in case df has a condition on it
  group_by <- rlang::enquos(...) %>% purrr::map(rlang::as_name) %>% unlist()
  rowsofdata <- dt[, .N] # faster than nrow() on big data.tables
  dt[, .N, by = group_by][,
    temp_prop := N/rowsofdata][,
    prop := round(temp_prop, digits = round)][
    order(-N)][, # sort in descending order by N before cumulative prop
    cum_prop := round(cumsum(temp_prop), digits = round)][,
    temp_prop := NULL][ # remove temp var
    order(-N)] # make sure final data.table sorted
}

#' @export
tab.tbl_df <- function(df, ..., round = 2) { # to check without requiring tibble
  group_by <- rlang::enquos(...)
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

#' @export
tab.data.frame <- function(df, ..., round = 2) { # to check without requiring tibble
  tab.data.table(data.table::as.data.table(df), ..., round)
}


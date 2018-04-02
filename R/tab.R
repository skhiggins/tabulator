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
#' @import dplyr
#' @import data.table
tab <- function(df, ..., round=2){ # note ... is the variable names to group by
  group_by <- dplyr::quos(...)
  if ("data.table" %in% class(df)) { # to check without requiring data.table
    group_by <- quo_to_chr(group_by)
    rowsofdata <- df[, .N] # faster than nrow() on big data.tables
    df[, .N, by = group_by][,
      temp_prop := N/rowsofdata][,
      prop := round(temp_prop, digits = round)][
      order(-N)][, # sort in descending order by N before cumulative prop
      cum_prop := round(cumsum(temp_prop), digits = round)][,
      temp_prop := NULL][ # remove temp var
      order(-N)] # make sure final data.table sorted
  } else if ("tbl_df" %in% class(df)) { # to check without requiring tibble
    rowsofdata <- nrow(df)
    df %>%
      group_by(!!!group_by) %>% # !!! since it's a quosure
      summarize(N = n()) %>%
      arrange(desc(N)) %>%
      ungroup() %>%
      mutate(
        temp_prop = N/rowsofdata,
        prop = round(temp_prop, digits = round),
        cum_prop = round(cumsum(temp_prop), digits = round)
      ) %>%
      select(-temp_prop)
  }
}


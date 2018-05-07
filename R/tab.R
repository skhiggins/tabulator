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

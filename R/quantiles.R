#' Efficient quantiles
#'
#' Produces quantiles of the variables.
#' \code{quantiles} shows quantile values.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{quantiles} uses \code{data.table} syntax.
#'
#' @param df A data.table or tibble
#' @param ... A column or set of columns (without quotation marks)
#' @return Quantile values
#' @examples
#' # data.table
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' a %>% quantiles(varname)
#'
#' # tibble
#' b <- tibble(varname = sample.int(20, size = 1000000, replace = TRUE))
#' b %>% quantiles(varname)
#'
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @export
quantiles <- function(df, ...) {
  UseMethod("quantiles", df)
  # to do: add round option like in tab()
}

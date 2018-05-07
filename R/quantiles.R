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

#' @export
quantiles.data.table <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  vars <- dplyr::quos(...) %>% quo_to_chr()
  tabbed <- df[, lapply(.SD, function(x) quantile(x, probs = probs, na.rm = na.rm)),
    .SDcols = vars
  ][, p := probs] %>% setcolorder(c("p", vars))
  tabbed[] # make sure it prints
}

#' @export
quantiles.tbl_df <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  vars <- dplyr::quos(...)
  df %>%
    dplyr::summarize(p = list(probs), q = list(quantile(!!!vars, probs))) %>%
    tidyr::unnest()
}

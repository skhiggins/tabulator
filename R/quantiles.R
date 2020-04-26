#' Efficient quantiles
#'
#' Produces quantiles of the variables.
#' \code{quantiles} shows quantile values.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{quantiles} uses \code{data.table} syntax.
#'
#' @param df A data.table, tibble, or data.frame
#' @param ... A column or set of columns (without quotation marks)
#' @param probs numeric vector of probabilities with values in [0,1].
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @return Quantile values
#' @examples
#' # data.table
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' a %>% quantiles(varname)
#'
#' # data.table: look at top 10% in more detail
#' a %>% quantiles(varname, probs = seq(0.9, 1, 0.01))
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
  vars <- rlang::enquos(...) %>% purrr::map(rlang::as_name) %>% unlist()
  tabbed <- df[, lapply(.SD, function(x) quantile(x, probs = probs, na.rm = na.rm)),
    .SDcols = vars
  ][, p := probs] %>% setcolorder(c("p", vars))
  tabbed[] # make sure it prints
}

#' @export
quantiles.tbl_df <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  vars <- rlang::enquos(...)
  df %>%
    dplyr::summarize(p = list(probs), q = list(quantile(!!!vars, probs))) %>%
    tidyr::unnest(cols = c(p, q))
}

#' @export
quantiles.data.frame <- function(df, ..., probs = seq(0, 1, 0.1), na.rm = FALSE) {
  quantiles.data.table(data.table::as.data.table(df), ..., probs = probs, na.rm = na.rm)
}


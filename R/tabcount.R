#' Count distinct categories
#'
#' Produces a count of unique categories,
#' \code{tabcount} shows the number of
#' 	unique categories for the selected variable.
#' 		Accepts data.table, tibble, or data.frame as input.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{tabcount} uses \code{data.table} syntax.
#'
#' @usage tabcount(df, ...)
#'
#' @param df A data.table, tibble, or data.frame
#' @param ... A column or set of columns (without quotation marks)
#'
#' @return Count of the number of unique groups formed by the variables given in \code{...} from \code{df}.
#'
#' @importFrom data.table data.table
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table setcolorder
#' @importFrom data.table .GRP
#' @importFrom data.table .N
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble
#' @importFrom stats quantile
#'
#' @examples
#' # data.table
#' library(data.table)
#' library(magrittr)
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' a %>% tabcount(varname)
#'
#' # tibble
#' library(dplyr)
#' b <- tibble(varname = sample.int(20, size = 1000000, replace = TRUE))
#' b %>% tabcount(varname)
#'
#' @export
tabcount <- function(df, ...) { # note ... is the variable names to group by
  UseMethod("tabcount", df)
}

#' @export
tabcount.data.table <- function(df, ...) {
  group_by <- rlang::enquos(...) %>% purrr::map(rlang::as_name) %>% unlist()
  df[, .N, by = group_by][, .N]
}

#' @export
tabcount.tbl_df <- function(df, ...) {
  group_by <- rlang::enquos(...)
  df %>%
    dplyr::distinct(!!!group_by) %>%  # !!! since it's a quosure
    nrow()
}

#' @export
tabcount.data.frame <- function(df, ...) {
  tabcount.data.table(data.table::as.data.table(df), ...)
}


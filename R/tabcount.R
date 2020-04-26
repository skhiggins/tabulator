#' Count distinct categories
#'
#' Produces a count of unique categories,
#' \code{tab} shows the number of
#' 	observations with that value, proportion of observations with that
#' 		value, and cumulative proportion, in descending order of frequency.
#' 		Accepts data.table, tibble, or data.frame as input.
#' Efficient with big data: if you give it a \code{data.table},
#' 		\code{tab} uses \code{data.table} syntax.
#'
#' @param df A data.table, tibble, or data.frame
#' @param ... A column or set of columns (without quotation marks)
#' @return Count of the number of unique groups formed by the variables given in \code{...} from \code{df}.
#' @examples
#' # data.table
#' a <- data.table(varname = sample.int(20, size = 1000000, replace = TRUE))
#' a %>% tabcount(varname)
#'
#' # tibble
#' b <- tibble(varname = sample.int(20, size = 1000000, replace = TRUE))
#' b %>% tabcount(varname)
#'
#' @importFrom magrittr %>%
#' @import data.table
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


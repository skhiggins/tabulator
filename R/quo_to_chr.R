#' Quosure to vector of strings
#'
#' Designed for functions that want a vector of strings as the variable name input
#'
#' @param x A quosure (\code{rlang::quos()})
quo_to_chr <- function(x) {
  toString(x) %>%
    stringr::str_replace_all("~","") %>% # remove the tilde in quosure
    stringr::str_replace_all("\\s","") %>% # remove spaces
    stringr::str_split(pattern = ",") %>%
    unlist()
}


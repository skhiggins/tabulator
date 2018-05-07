tabcount.data.frame <- function(df, ...) {
  group_by <- quo_to_chr(group_by)
  length(unique(df[[group_by]]))
}

#' Return equivalent name from a \code{data.frame} of equivalencies
#'
#' This is simply a wrapper around \code{match} for a specific \code{data.frame} of values.
#'
#' @param value   Vector of values to match in \code{df}.
#' @param df      A \code{data.frame} where every row is a set of equivalent names.
#' @param column  A character string or numeric of length 1, indicating the column
#'                in \code{df} to return names from.
#'
#' @export
equivalentName <- function(value, df, column) {
  out <- lapply(df, function(x) match(as.character(value), x))
  likelyMatch <- which.max(unlist(lapply(out, function(x) sum(!is.na(x)))))
  df[[column]][out[[names(likelyMatch)]]]
}

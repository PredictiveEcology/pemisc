#' Convert a character string to sentence case
#'
#' @param x A character string.
#'
#' @export
toSentenceCase <- function(x) {
  newNames <- tolower(x)
  substr(newNames, 1, 1) <- toupper(substr(newNames, 1, 1))
  newNames
}

#' Extract terms in a quoted model statement
#'
#' Similar to `terms`, but this is used on a quoted model and
#' will only return unique matches in a `data`.
#'
#' @param model A quoted model statement
#' @param data A data.frame-like object with column names in which to match terms in
#'   `model`
#' @export
termsInData <- function(model, data) {
  terms <- strsplit(gsub(" ", "", as.character(model)), split = "[[:punct:]]+")[[2]][-1] # remove response
  terms <- unique(terms)
  terms <- terms[terms %in% colnames(data)]
}

#' Use message to print a clean, rectangular data structure
#'
#' Sends to `message`, but in a structured way so that a `data.frame`-like can
#' be cleanly sent to messaging.
#'
#' @param df A data.frame, data.table, matrix
#' @param round An optional numeric to pass to `round`
#' @param colour An optional colour to use from `crayon`
#'
#' @export
#' @importFrom data.table as.data.table is.data.table set
#' @importFrom utils capture.output
messageDF <- function(df, round, colour = NULL) {
  if (is.matrix(df))
    df <- as.data.frame(df)
  if (!is.data.table(df)) {
    df <- as.data.table(df)
  }
  if (!missing(round)) {
    isNum <- sapply(df, is.numeric)
    isNum <- colnames(df)[isNum]
    for (Col in isNum) {
      set(df, NULL, Col, round(df[[Col]], round))
    }
  }
  out <- lapply(capture.output(df), function(x) {
    if (!is.null(colour)) {
      message(getFromNamespace(colour, ns = "crayon")(x))
    } else {
      message(x)
    }
  })
}

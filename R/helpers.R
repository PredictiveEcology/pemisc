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

#' Determine name of current user
#'
#' @param username Character.
#'
#' @return Logical indicating whether the current user matches \code{username};
#' or, if \code{username} is NULL, a character string indicating the current user.
#'
#' @export
user <- function(username = NULL) {
  if (is.null(username)) {
    Sys.info()[["user"]]
  } else {
    identical(username, Sys.info()[["user"]])
  }
}

#' Write sim event info to file
#'
#' Useful for debugging.
#'
#' @param file Character specifying the filename (default \code{"events.txt"}).
#' @param append  Logical indicating whether to append to the file (default \code{FALSE}).
#'
#' @return Nothing returned. Invoked for its side-effect of writing to file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible normPath
#' @importFrom SpaDES.core current
writeEventInfo <- function(sim, file = "events.txt", append = FALSE) {
  f <- normPath(file)
  curr <- current(sim)
  cat(paste(curr$moduleName, curr$eventType, curr$eventTime), ":\n",
      file = file, append = append)
}

#' Write RNG state info to file
#'
#' Useful for debugging and ensuring reproducibility.
#'
#' @param file Character specifying the filename (default \code{"seed.txt"}).
#' @param append  Logical indicating whether to append to the file (default \code{FALSE}).
#'
#' @return Nothing returned. Invoked for its side-effect of writing to file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible normPath
writeRNGInfo <- function(file = "seed.txt", append = FALSE) {
  fseed <- normPath(file)
  cat("\tStart of new RNG stream: ", file = fseed, append = append)
  ## NOTE: the first element of .Random.seed specifies the RNG type, so omit it
  cat(.Random.seed[2:11], file = fseed, sep = ", ", append = append)
  cat(".", file = fseed, sep = "\n", append = append)
}

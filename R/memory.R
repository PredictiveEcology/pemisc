#' Report the estimated amount of available memory in the OS
#'
#' This reports the 'available' memory from a system \code{free} call.
#' If \code{free} is not installed on the system (e.g., non-Linux), returns \code{NULL}.
#'
#' @return Numeric of class "object_size", so it can be reported in any units with format,
#' e.g., \code{format(availableMemory(), unit = "GB")}.
#'
#' @export
#' @seealso \code{man free} for description of available memory estimation.
availableMemory <- function() {
  aa <- try(system("free -b", intern = TRUE), silent = TRUE)
  availMem <- if (!is(aa, "try-error")) {
    ## take the 'available' column from the 'Mem:' row of the table
    bb <- strsplit(aa[2], split = " ")
    as.numeric(bb[[1]][nzchar(bb[[1]])][7])
  } else {
    NULL
  }

  if (!is.null(availMem)) {
    class(availMem) <- "object_size"
  } else {
    warning("unable to determine available memory.",
            " 'free' must be available on your system (typically Linux-only).")
  }
  return(availMem)
}

#' Report the estimated amount of available memory in the OS
#'
#' This uses \code{system("free -lb", intern = TRUE))}, which will
#' only work on Linux-alikes. On if the program 'free' is not installed
#' on the system, this function returns \code{NULL}
#'
#' @export
#' @return
#' Returns a numeric, of class "object_size", so it can be reported
#' in any units with format, e.g.,
#' \code{format(availableMemory(), unit = "GB")}
availableMemory <- function() {
  try(aa <- system("free -lb", intern = TRUE))
  availMem <- if (!is(aa, "try-error")) {
    bb <- strsplit(aa[2], split = " ")
    as.numeric(bb[[1]][nzchar(bb[[1]])][7])
  } else {
    NULL
  }
  class(availMem) <- "object_size"
  return(availMem)
}

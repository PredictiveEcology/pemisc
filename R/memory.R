#' Report the estimated amount of available memory in the OS
#'
#' This reports the 'available' memory from a system call: \code{free} on Linux,
#' \code{vm_stat} on macOS, or \code{wmic} on Windows.
#' If neither is installed on the system, returns \code{NULL}.
#'
#' @return Numeric of class "object_size", so it can be reported in any units with format,
#' e.g., \code{format(availableMemory(), unit = "GB")}.
#'
#' @export
#' @importFrom magrittr %>%
#' @seealso \code{man free} for description of available memory estimation.
availableMemory <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    wmic <- Sys.which("wmic")
    aa <- try(system(paste0(wmic, " OS get FreePhysicalMemory"), intern = TRUE))
    suppressWarnings({
      availMem <- if (!is(aa, "try-error")) {
        aa <- gsub(" |(\r)", "", aa);
        na.omit(as.numeric(aa)) * 1e3 # in KB!
      } else {
        NULL
      }
    })
  } else {
    free <- Sys.which("free")      ## Linux only
    vmstat <- Sys.which("vm_stat") ## macOS

    if (nzchar(free)) {
      aa <- try(system(paste(free, "-b"), intern = TRUE), silent = TRUE)
      availMem <- if (!is(aa, "try-error")) {
        ## take the 'available' column from the 'Mem:' row of the table
        bb <- strsplit(aa[2], split = " ")
        as.numeric(bb[[1]][nzchar(bb[[1]])][7])
      }
    } else if (nzchar(vmstat)) {
      aa <- try(system(vmstat, intern = TRUE), silent = TRUE)
      availMem <- if (!is(aa, "try-error")) {
        ## take the 'Pages free' row value times by the page size value
        pageSize <- strsplit(aa[1], split = "page size of ")[[1]][2] %>%
          strsplit(., split = " bytes)") %>%
          `[[`(., 1) %>%
          as.numeric()
        bb <- strsplit(aa[2], split = " ")[[1]] %>%
          unique()
        pagesFree <- bb[which(!(bb %in% c("Pages", "free:", "")))] %>%
          as.numeric()
        pagesFree * pageSize ## in bytes
      }
    } else {
      availMem <- NULL
    }
  }

  if (!is.null(availMem)) {
    class(availMem) <- "object_size"
  } else {
    warning("unable to determine available memory.",
            "'wmic' (Windows), 'free' or 'vm_stat' must be available on your system.")
  }
  return(availMem)
}

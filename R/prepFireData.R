#' Download the National Burn Area Composite (Fires) in Canada
#'
#' This runs \code{prepInputs} internally, so use can pass
#' \code{studyArea} etc.
#'
#' @param year Numeric, length 1. Which year, from 1986 to 2018 (currently)
#'   to download
#' @param urlBase The url of the directory where the NBAC are stored. Default
#'   is the currently known url. If this url becomes stale, please notify
#'   the predictive ecology team.
#' @return
#' A SpatialPolygonsDataFrame plus several downloaded files, including
#' the .zip archive and the extracted files. Because it is running
#' \code{prepInputs}, checksumming is occurring too.
#' @examples
#' \dontrun{
#'   # This will download 2 recent years
#'   NBAC <- lapply(2016:2017, function(yr) a <- prepFireNBAC(yr))
#' }
prepFireNBAC <- function(year, urlBase = "http://cwfis.cfs.nrcan.gc.ca/downloads/nbac/",
                         ...) {
  a <- Cache(getURL, urlBase,
              verbose=TRUE,
              dirlistonly = TRUE, notOlderThan = Sys.time() - (1440 *60))
  lineWithFile <- grep(paste0("nbac_", year, ".*\\.zip"), strsplit(a, "\n")[[1]], value = TRUE)
  #gsub(".*href=\\\"(.*\\.zip)", "\\1", lineWithFile)
  filename <- gsub(".*>(.*\\.zip)<.*", "\\1", lineWithFile)
  if (length(filename) > 0) {
    prepInputs(url = file.path(urlBase, filename), archive = filename,
               ...)
  } else {
    message("There is no NBAC file for ", year)
    NULL
  }
}

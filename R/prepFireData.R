#' Download the National Burn Area Composite (Fires) in Canada
#'
#' Downloads data from CWFIS Datamart at
#' \url{http://cwfis.cfs.nrcan.gc.ca/datamart}.
#' This runs \code{prepInputs} internally, so use can pass
#' \code{studyArea} etc.
#'
#' @param year Numeric, length 1. Which year, from 1986 to 2018 (currently)
#'   to download
#' @param type Either "NBAC", "Polygon" or "Point" to get the
#'   National Burn Area Composite, the Polygon or the Point datasets.
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
#'   library(sf)
#'   NBAC <- lapply(2016:2017, function(yr) a <- prepFireCanada(yr))
#'   Points <- prepFireCanada(yr, type = "Points", fun = "st_read")
#'   Polygons <- prepFireCanada(yr, type = "Polygons")
#' }
#' @importFrom reproducible prepInputs
#' @importFrom RCurl getURL
prepFireCanada <- function(year, type = c("NBAC", "Polygon", "Point"),
                           urlBase = "http://cwfis.cfs.nrcan.gc.ca/downloads/nbac/",
                         ...) {
  if (length(type) > 1)
    type = type[1]

  possTypes <- c("NBAC", "Polygons", "Points")
  type <- possTypes[pmatch(tolower(type), tolower(possTypes))]
  if (identical(type, possTypes[1])) {
    a <- Cache(getURL, urlBase,
               verbose=TRUE,
               dirlistonly = TRUE, notOlderThan = Sys.time() - (1440 *60))
    lineWithFile <- grep(paste0("nbac_", year, ".*\\.zip"), strsplit(a, "\n")[[1]], value = TRUE)
    #gsub(".*href=\\\"(.*\\.zip)", "\\1", lineWithFile)
    filename <- gsub(".*>(.*\\.zip)<.*", "\\1", lineWithFile)
    if (length(filename) > 0) {
      out <- prepInputs(url = file.path(urlBase, filename), archive = filename,
                 ...)
    } else {
      message("There is no NBAC file for ", year)
      out <- NULL
    }
  } else if (identical(type, possTypes[3])) {
    out <- prepInputs(url =
    "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
    archive = "NFDB_point.zip",
    ...)
  } else if (identical(type, possTypes[2])) {
    out <- prepInputs(url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
                      archive = "NFDB_poly.zip", ...)
  }

  return(out)
}




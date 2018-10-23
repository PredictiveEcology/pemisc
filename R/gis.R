#' Create a \code{.prj} file
#'
#' In cases where a shapefile is missing its associated \code{.prj} file.
#'
#' @param shpFile The filename of a shapefile to add \code{.prj}
#' @param urlForProj The url from which to fetch the projection, e.g.,
#'          \code{"http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/"}.
#'
#' @export
#' @importFrom tools file_path_sans_ext
createPrjFile <- function(shpFile,
                          urlForProj = "http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/") {
  basenameWithoutExt <- file_path_sans_ext(shpFile)
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file(urlForProj, destfile = prjFile)
  }
}

#' Do an arbitrary set of operations on a polygon
#'
#' @param shp  A polygon object, or a character string identifying the shapefile
#'             path to load, and clean.
#' @param fn   A function identifying the type of cleaning to do.
#' @param type If fn is not known, an character string can be specified to
#'             identify which \code{fn} to use.
#'             This MUST be a known type for this function.
#' @param ...  Passed to \code{fn}
#'
#' @export
polygonClean <- function(poly, fn = NULL, type = NULL, ...) {
  if (is.null(fn)) {
    if (is.null(type)) {
      stop("Either fn or type must be specified")
    } else {
      if (length(na.omit(pmatch(c("LandWeb", "tolko", "testing"), type))))
        fn <- .cleanLandWebStudyArea
    }
  }
  poly <- fn(poly, ...)
}

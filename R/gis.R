#' Create a .prj file
#'
#' In cases where a shapefile is missing its associated .prj file.
#'
#' @param shpFile The filename of a shapefile to add .prj
#' @param prjFile The filename of a
createPrjFile <- function(shpFile, prjFile) {
  basenameWithoutExt <- tools::file_path_sans_ext(shpFile)
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file("http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/",
                  destfile = prjFile)
  }
}


#' Do an arbitrary set of operations on a polygon
#'
#' @param shp A polygon object, or a character string identifying the shapefile path
#'            to load, and clean.
#' @param fn A function identifying the type of cleaning to do.
#' @param type If fn is not known, an character string can be specified to
#'             identify which \code{fn} to use. This MUST
#'             be a known type for this function.
polygonClean <- function(poly, fn = NULL, type = NULL) {
  if (is.null(fn)) {
    if (is.null(type)) {
      stop("Either fn or type must be specified")
    } else {
      if (isTRUE(grepl("LandWeb", type)))
        fn <- .cleanLandWebStudyArea
    }
  }
  poly <- fn(poly)
}

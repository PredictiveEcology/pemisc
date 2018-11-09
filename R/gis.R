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
#' @importFrom utils download.file
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
#' @importFrom stats na.omit
polygonClean <- function(poly, fn = NULL, type = NULL, ...) {
  if (is.null(fn)) {
    if (is.null(type)) {
      stop("Either fn or type must be specified")
    } else {
      if (length(na.omit(pmatch(c("LandWeb", "tolko", "LP", "testing"), type))))
        fn <- .cleanLandWebStudyArea
      else
        stop("Unknown type")
    }
  }
  poly <- fn(poly, ...)
}

#' Make a vegetation type map from a stack of species abundances
#'
#' @param speciesStack A Raster Stack of species abundances. This must be one Raster Layer
#'        per species.
#' @param vegLeadingProportion The threshold as a proportion of the total abundance
#'        that a species must have to be considered a "pure" stand of that type.
#'        If no species reaches this proportion, then the pixel will be 'Mixed'.
#' @param mixed Logical. If \code{TRUE}, then a mixed pixel value will be identified and given
#'        (see \code{vegLeadingProportion} argument)
#' @return A factor raster
#'
#' @export
#' @importFrom raster maxValue which.max
makeVegTypeMap <- function(speciesStack, vegLeadingProportion, mixed = TRUE) {
  sumVegPct <- sum(speciesStack, na.rm = TRUE)

  # create "mixed" layer, which is given a value slightly higher than any other layer
  #   if it is deemed a mixed pixel
  speciesStack$Mixed <- all(speciesStack / sumVegPct < vegLeadingProportion) *
    max(maxValue(speciesStack))*1.01
  vegTypeMap <- raster::which.max(speciesStack)
  layerNames <- names(speciesStack)
  names(layerNames) <- layerNames
  levels(vegTypeMap) <- data.frame(ID = seq(layerNames), Species = names(layerNames))
  vegTypeMap
}

#' Faster version of \code{\link[raster]{factorValues}}
#'
#' @inheritParams raster::factorValues
#' @importFrom raster levels
#' @export
factorValues2 <- function(x, v, layer, att, append.names) {
  levs <- raster::levels(x)[[1]];
  a <- match(na.omit(v), levs$ID);
  levs[[att]][a]

}

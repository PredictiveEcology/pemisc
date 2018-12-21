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
                          urlForProj = "http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/") {  #nolint
  basenameWithoutExt <- file_path_sans_ext(shpFile)
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file(urlForProj, destfile = prjFile)
  }
}

#' Faster version of \code{\link[raster]{factorValues}}
#'
#' Note there is an option to remove the NAs, which will make it MUCH faster,
#' if \code{TRUE}
#'
#' @inheritParams raster::factorValues
#'
#' @param na.rm Logical. If \code{TRUE}, then the NAs will be removed, returning a possibly
#'              shorter vector
#' @export
#' @importFrom raster levels
#' @importFrom stats na.omit
factorValues2 <- function(x, v, layer, att, append.names, na.rm = FALSE) {
  levs <- raster::levels(x)[[1]];
  if (isTRUE(na.rm))
    v <- na.omit(v)
  a <- match(v, levs$ID);
  levs[[att]][a]
}

#' Extract or create a raster to match
#'
#' This extracts or creates a new raster layer, whose intention is to be used as
#' the \code{rasterToMatch} argument in further \code{prepInputs} calls.
#'
#' @param x A Raster Layer with correct resolution and origin.
#' @param ... Additional arguments
#'
#' @return A \code{RasterLayer} object.
#'
#' @export
#' @exportMethod rasterToMatch
#' @rdname rasterToMatch
setGeneric(
  "rasterToMatch",
  function(x, ...) {
    standardGeneric("rasterToMatch")
})

#' @param studyArea A SpatialPolygon* object that will be sent to \code{postProcess}.
#'
#' @export
#' @exportMethod rasterToMatch
#' @importFrom raster raster setValues
#' @importFrom reproducible postProcess
#' @rdname rasterToMatch
setMethod("rasterToMatch", signature = "Raster",
          definition = function(x, studyArea, ...) {
            rtm <- raster::raster(x)
            rtm <- setValues(rtm, 1L)
            postProcess(rtm, studyArea = studyArea, ...)
})

#' @param rasterToMatch The raster to match in a \code{fasterize} call.
#'
#' @export
#' @exportMethod rasterToMatch
#' @importFrom fasterize fasterize
#' @rdname rasterToMatch
setMethod("rasterToMatch", signature = "SpatialPolygonsDataFrame",
          definition = function(x, studyArea, rasterToMatch, ...) {
            numPolys <- length(x)
            xDF <- as.data.frame(x)
            x$numPolys <- seq_len(numPolys)
            xDF <- data.frame(ID = x$numPolys, xDF)
            rtm <- fasterize::fasterize(sf::st_as_sf(x),
                                        field = "numPolys",
                                        rasterToMatch)
            levels(rtm) <- xDF
            rtm[is.na(rasterToMatch[])] <- NA
            rtm
})

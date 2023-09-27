#' Create a `.prj` file
#'
#' In cases where a shapefile is missing its associated `.prj` file.
#'
#' @param shpFile The filename of a shapefile to add `.prj`
#' @param urlForProj The url from which to fetch the projection, e.g.,
#'          `"http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/"`.
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

#' Faster version of [raster::factorValues()]
#'
#' Note there is an option to remove the NAs, which will make it MUCH faster,
#' if `TRUE`
#'
#' @inheritParams raster::factorValues
#'
#' @param na.rm Logical. If `TRUE`, then the NAs will be removed, returning a possibly
#'              shorter vector
#' @export
#' @importFrom raster levels
#' @importFrom stats na.omit
factorValues2 <- function(x, v, layer, att, append.names, na.rm = FALSE) {
  if (is(x, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("please install.packages('terra')")
    levs <- terra::cats(x)[[1]]
  } else {
    levs <- raster::levels(x)[[1]]
  }
  idCol <- grep("id", ignore.case = TRUE, value = TRUE, colnames(levs))
  if (length(idCol) == 0) {
    idCol <- grep("value", ignore.case = TRUE, value = TRUE, colnames(levs))
  }
  if (isTRUE(na.rm))
    v <- na.omit(v)
  a <- match(v, levs[[idCol]])
  return(levs[[att]][a])
}

#' Extract or create a raster to match
#'
#' This extracts or creates a new raster layer, whose intention is to be used as
#' the `rasterToMatch` argument in further `prepInputs` calls.
#'
#' @param x A Raster Layer with correct resolution and origin.
#' @param ... Additional arguments
#'
#' @return A `RasterLayer` object.
#'
#' @export
#' @exportMethod rasterToMatch
#' @rdname rasterToMatch
setGeneric(
  "rasterToMatch",
  function(x, ...) {
    standardGeneric("rasterToMatch")
})

#' @param studyArea A `SpatialPolygon*` object that will be sent to `postProcess`.
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

#' @param rasterToMatch The raster to match in a `fasterize` call.
#'
#' @export
#' @exportMethod rasterToMatch
#' @importFrom fasterize fasterize
#' @importFrom sf st_as_sf
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

#' Normalize each layer of a `RasterStack`
#'
#' Rescales the values of of each `RasterLayer` between `[0,1]`.
#'
#' @param x A `RasterStack` object.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom amc rescale
#' @importFrom raster stack
normalizeStack <- function(x) {
  normalized <- lapply(names(x), function(layer) {
    lay <- amc::rescale(x[[layer]])
    names(lay) <- layer
    return(lay)
  })
  names(normalized) <- names(x)
  return(raster::stack(normalized))
}

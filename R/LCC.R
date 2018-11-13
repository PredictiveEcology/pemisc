#' Define flammability map
#'
#' @param LandCoverClassifiedMap A \code{Raster} that represents land cover
#'             (e.g., Land Cover Classified map from 2005 or 2010 from the Canadian Forest Service).
#'
#' @param nonFlammClasses numeric vector defining which classes in \code{LandCoverClassifiedMap}.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom raster maxValue minValue ratify reclassify writeRaster
#' @importFrom quickPlot setColors<-
#' @export
defineFlammable <- function(LandCoverClassifiedMap = NULL,
                            nonFlammClasses = c(36L, 37L, 38L, 39L),
                            mask = NULL, filename2 = NULL) {
  if (!is.null(mask))
    if (!is(mask, "Raster")) stop("mask must be a raster layer")
  if (!is(LandCoverClassifiedMap, "RasterLayer")) {
    stop("Need a classified land cover map. Currently only accepts 'LCC2005'")
  }
  if (is.null(nonFlammClasses))
    stop("Need nonFlammClasses, which are the classes that cannot burn in the LandCoverClassifiedMap")

  oldClass <- minValue(LandCoverClassifiedMap):maxValue(LandCoverClassifiedMap)
  newClass <- ifelse(oldClass %in% nonFlammClasses, 0L, 1L) ## NOTE: 0 codes for NON-flammable
  #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  #according to Yong, Canada Landcover 2005 is loaded as LandCoverClassifiedMap
  rstFlammable <- ratify(reclassify(LandCoverClassifiedMap, flammableTable, count = TRUE))
  if (!is.null(filename2))
    rstFlammable <- writeRaster(rstFlammable, filename = filename2, overwrite = TRUE)

  setColors(rstFlammable, n = 2) <- colorRampPalette(c("blue", "red"))(2)
  if (!is.null(mask)) rstFlammable[is.na(mask[])] <- NA_integer_

  rstFlammable[] <- as.integer(rstFlammable[])
  rstFlammable
}

#' Simple prepInputs for LCC2005 or LCC2010
#'
#' A wrapper around prepInputs for the Canadian Land Cover Classification product(s)
#'
#' @inheritParams reproducible::cropInputs
#'
#' @param year Numeric, either 2005 or 2010 (not yet implemented)
#'
#' @export
#' @importFrom reproducible asPath prepInputs
prepInputsLCC <- function(year = 2005,
                          destinationPath = ".",
                          studyArea = NULL,
                          rasterToMatch = NULL,
                          filename2 = NULL, ...) {

  dots <- list(...)
  if (is.null(dots$url)) {
    if (identical(as.integer(year), 2005L)) {
      url <- "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"
      LCCfilename <- asPath("LCC2005_V1_4a.tif")
    } else {
      stop("don't have the url for LCC2010 yet. Please find it and pass it as url = 'TheFTPAddress'")
    }
  }

  prepInputs(targetFile = LCCfilename,
             archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
             url = url,
             destinationPath = destinationPath,
             studyArea = studyArea,
             rasterToMatch = rasterToMatch,
             method = "bilinear",
             datatype = "INT2U",
             filename2 = filename2, ...)
}


#' @export
#' @exportMethod rasterToMatch
setGeneric(
  "rasterToMatch",
  function(x, ...) {
    standardGeneric("rasterToMatch")
})

#' Simple wrapper around \code{postProcess}
#'
#' This creates a new raster layer, whose intention is to be used as
#' the \code{rasterToMatch} argument in further \code{prepInputs} calls.
#'
#' @param x A Raster Layer with correct resolution and origin
#' @param studyArea A SpatialPolygon* object that will be sent to \code{postProcess}
#' @param rasterToMatch The raster to match in a \code{fasterize} call
#'
#' @return A RasterLayer object.
#'
#' @export
#' @exportMethod rasterToMatch
#' @importMethodsFrom map rasterToMatch
#' @importFrom raster raster setValues
#' @importFrom reproducible postProcess
#'
setMethod("rasterToMatch", signature = "Raster",
          definition = function(x, studyArea, ...) {
            rtm <- raster::raster(x)
            rtm <- setValues(rtm, 1L)
            postProcess(rtm, studyArea = studyArea, ...)
})

#' @export
#' @exportMethod rasterToMatch
#' @importFrom fasterize fasterize
setMethod("rasterToMatch", signature = "SpatialPolygonsDataFrame",
          definition = function(x, studyArea, rasterToMatch, ...) {
            numPolys <- length(x)
            xDF <- as.data.frame(x)
            x$numPolys <- seq_len(numPolys)
            xDF <- data.frame(ID = x$numPolys, xDF)

            rtm <- fasterize::fasterize(sf::st_as_sf(x), field = "numPolys",
                                        rasterToMatch)
            levels(rtm) <- xDF
            rtm
})

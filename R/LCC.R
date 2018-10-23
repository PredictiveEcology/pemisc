#' Define flammability map
#'
#' @param LandCoverClassifiedMap A \code{Raster} that represents land cover
#'             (e.g., Land Cover Classified map from 2005 or 2010 from the Canadian Forest Service).
#'
#' @param nonFlammClasses numeric vector defining which classes in \code{LandCoverClassifiedMap}.
#'
#' @importFrom raster ratify reclassify writeRaster
defineFlammable <- function(LandCoverClassifiedMap = NULL, nonFlammClasses = NULL) {
  if (is.null(LandCoverClassifiedMap))
    stop("Need a classified land cover map. Try running prepInputs(...)")
  if (is.null(nonFlammClasses))
    nonFlammClasses <- c(36L, 37L, 38L, 39L)

  oldClass <- 0:39 ## TODO: this is LCC specific; needs to be general
  newClass <- ifelse(oldClass %in% nonFlammClasses, 1, 0)   #1 codes for non flammable ## TODO: fix this
  #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  #according to Yong, Canada Landcover 2005 is loaded as LandCoverClassifiedMap
  rstFlammable <- ratify(reclassify(LandCoverClassifiedMap, flammableTable, count = TRUE))
  rstFlammable <- writeRaster(rstFlammable,
                              filename = file.path(outputPath(sim), "rstFlammable"),
                              overwrite = TRUE)

  setColors(rstFlammable, n = 2) <- colorRampPalette(c("blue", "red"))(2)
  rstFlammable[is.na(rstStudyRegion[])] <- NA
  rstFlammable
}


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

#' Simple wrapper around \code{postProcess}
#'
#' This creates a new raster layer, whose intention is to be used as
#' the \code{rasterToMatch} argument in further \code{prepInputs} calls
#' @param raster A Raster Layer with correct resolution and origin
#' @param studyArea A SpatialPolygon* object that will be sent to \code{postProcess}
#'
#' @return A RasterLayer object.
#'
#' @export
rasterToMatch <- function(raster, studyArea, ...) {
  rtm <- raster::raster(raster)
  rtm <- setValues(rtm, 1L)
  postProcess(x = rtm, studyArea = studyArea, ...)
}

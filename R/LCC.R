#' @param LandCoverClassifiedMap A Raster that represents land cover, e.g., Land Cover
#'             Classified map from 2010 or 2005 from the Canadian Forest Service
#' @param
defineFlammable <- function(LandCoverClassifiedMap = NULL, nonFlammClasses = NULL) {
  if (is.null(LandCoverClassifiedMap))
    stop("Need a classified land cover map. Try running prepInputs(...)")
  if (is.null(nonFlammClasses))
    nonFlammClasses <- c(36, 37, 38, 39)

  oldClass <- 0:39
  newClass <-
    ifelse(oldClass %in% nonFlammClasses, 1, 0)   #1 codes for non flammable
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

#' Define flammability map
#'
#' @param LandCoverClassifiedMap A \code{Raster} that represents land cover
#'             (e.g., Land Cover Classified map from 2005 or 2010 from the Canadian Forest Service).
#'
#' @param nonFlammClasses numeric vector defining which classes in \code{LandCoverClassifiedMap}.
#'
#' @param mask A raster to use as a mask (see \code{\link[raster]{mask}}).
#'
#' @param filename2 See \code{link[reproducible]{postProcess}}. Default \code{NULL}.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom quickPlot setColors<-
#' @importFrom raster maxValue minValue ratify reclassify writeRaster
#' @export
defineFlammable <- function(LandCoverClassifiedMap = NULL,
                            nonFlammClasses = c(36L, 37L, 38L, 39L),
                            mask = NULL, filename2 = NULL) {
  if (!is.null(mask))
    if (!is(mask, "Raster")) stop("mask must be a raster layer")
  if (!is(LandCoverClassifiedMap, "RasterLayer")) {
    stop("Need a classified land cover map. Currently only accepts 'LCC2005'")
  }
  if (!is.integer(LandCoverClassifiedMap[]))
    stop("LandCoverCLassifiedMap must be an integer")
  if (is.null(nonFlammClasses))
    stop("Need nonFlammClasses, which are the classes that cannot burn in the LandCoverClassifiedMap")

  oldClass <- minValue(LandCoverClassifiedMap):maxValue(LandCoverClassifiedMap)
  newClass <- ifelse(oldClass %in% nonFlammClasses, 0L, 1L) ## NOTE: 0 codes for NON-flammable
  #see mask argument for SpaDES::spread()
  flammableTable <- cbind(oldClass, newClass)
  #according to Yong, Canada Landcover 2005 is loaded as LandCoverClassifiedMap
  rstFlammable <- ratify(reclassify(LandCoverClassifiedMap, flammableTable))
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
#' @inheritParams reproducible::postProcess
#' @inheritParams reproducible::prepInputs
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

  Cache(prepInputs, targetFile = LCCfilename,
        archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
        url = url,
        destinationPath = destinationPath,
        studyArea = studyArea,
        rasterToMatch = rasterToMatch,
        method = "bilinear",
        datatype = "INT2U",
        filename2 = filename2, ...)
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
    max(maxValue(speciesStack)) * 1.01
  vegTypeMap <- raster::which.max(speciesStack)
  layerNames <- names(speciesStack)
  names(layerNames) <- layerNames
  levels(vegTypeMap) <- data.frame(ID = seq(layerNames), Species = names(layerNames))
  vegTypeMap
}

#' Function to load kNN species layers from online data repository
#'
#' TODO: description needed
#'
#' @param dPath path to the data directory
#' @param rasterToMatch passed to \code{link[reproducible]{prepInputs}}
#' @param studyArea passed to \code{link[reproducible]{prepInputs}}
#' @param sppNameVector a character vector of species names to download, in their final form
#' @param sppMerge a list of species names (as in \code{sppNameVector}) to which correspond
#' more than one Knn layers that should be overlaid to produce a single species layer. List
#' \code{names} should be the final desired names, whereas list entries will ideally match the Knn
#' name format. However, \code{link[pemisc]{equivalentName}} is used internally to conform list names to
#' LandR naming and list entries to Knn naming using \code{{link[pemisc]speciesEquivalencies}}.
#' @param speciesEquivalency table with species name equivalencies between the Knn format
#' and the final naming format
#' @param knnNamesCol character string indicating the column in \code{speciesEquivalency}
#' containing Knn species names
#' @param sppEndNamesCol character string indicating the column in \code{speciesEquivalency}
#' to use for final species names
#' @param sppMerge list of Knn species' layers that should be merged. List names correspond to final species
#' names as in \code{sppEndNamesCol}, list entries correspond to Knn species layers to be merged. Defaults to NULL
#' @param thresh the minimum number of pixels where the species must have
#'               \code{biomass > 0} to be considered present in the study area.
#'               Defaults to 1.
#' @param url the source url for the data, passed to \code{link[reproducible]{prepInputs}}
#' @param cachePath path to the cache directory
#' @param ... Additonal arguments passed to \code{link[reproducible]{Cache}} and \code{link[pemisc]{equivalentName}}
#'
#' @return a list of two elements: \code{speciesLayer}, a raster stack; and
#'         \code{speciesList}, a vector(?) of species names.
#'
#' @export
#' @importFrom raster ncell
#' @importFrom reproducible Cache
#' @importFrom utils untar
#'
loadkNNSpeciesLayers <- function(dPath, rasterToMatch, studyArea, sppNameVector, speciesEquivalency,
                                 knnNamesCol, sppEndNamesCol, sppMerge = NULL, thresh = 1, url, cachePath, ...) {
  dots <- list(...)

  ## get .tar file first - no extraction
  outPreProcess <- preProcess(targetFile = file.path(dPath, "kNN-Species.tar"), archive = file.path(dPath, "kNN-Species.tar"),
                              url = url, destinationPath = dPath)

  ## get all kNN species
  allSpp <- Cache(untar, tarfile = outPreProcess$targetFilePath, list = TRUE)
  allSpp <- allSpp %>%
    grep(".zip", ., value = TRUE) %>%
    sub("_v0.zip", "", .) %>%
    sub(".*Species_", "", .)

  if (sppNameVector == "all") {
    ## get all species layers from .tar
    sppNameVector <- allSpp
    }

  ## Make sure spp names are compatible with kNN names
  KnnNames <- as.character(equivalentName(sppNameVector, speciesEquivalency, column = knnNamesCol))

  if(any(is.na(KnnNames))) {
    warning(paste0("Can't find ", sppNameVector[is.na(KnnNames)], " in `speciesEquivalency$",
            knnNamesCol, ".\n Will use remaining matching species, but check if this is correct"))
    KnnNames <- KnnNames[!is.na(KnnNames)]
    sppNameVector <- sppNameVector[!is.na(KnnNames)]
  }


  if(any(!KnnNames %in% allSpp)) {
    warning(paste0("Can't find ", sppNameVector[is.na(KnnNames)], " in  in kNN database.
                   \n Will use remaining matching species, but check if this is correct"))
    KnnNames <- KnnNames[KnnNames %in% allSpp]
    sppNameVector <- sppNameVector[KnnNames %in% allSpp]
  }

  ## define suffix to append to file names
  suffix <- if (basename(cachePath) == "cache") {
    paste0(as.character(ncell(rasterToMatch)), "px")
  } else {
    basename(cachePath)
  }
  suffix <- paste0("_", suffix)

  ## select which archives/targetFiles to extract
  targetFiles <- paste0("NFI_MODIS250m_kNN_Species_", KnnNames, "_v0.tif")
  names(targetFiles) <- targetFiles
  archives <- cbind(archive1 = file.path(dPath, "kNN-Species.tar"),
                    archive2 = paste0("NFI_MODIS250m_kNN_Species_", KnnNames, "_v0.zip"))
  archives <- split(archives, archives[, "archive2"])

  postProcessedFilenames <- .suffix(targetFiles, suffix = suffix)

  speciesLayers <- Map(targetFile = targetFiles, archive = archives,
                       filename2 = postProcessedFilenames,
                       MoreArgs = list(url = url,
                                       destinationPath = asPath(dPath),
                                       fun = "raster::raster",
                                       studyArea = studyArea,
                                       rasterToMatch = rasterToMatch,
                                       method = "bilinear",
                                       datatype = "INT2U",
                                       overwrite = TRUE,
                                       userTags = dots$userTags
                       ),
                       prepInputs)

  names(speciesLayers) <- KnnNames

  ## Sum species that share same final name
  if (!is.null(sppMerge)) {
    ## make sure species names and list names are in the right formats
    sppMerge <- lapply(sppMerge, FUN = function(x) equivalentName(x, speciesEquivalency,  column = knnNamesCol))
    names(sppMerge) <- equivalentName(names(sppMerge), speciesEquivalency,  column = sppEndNamesCol)

    ## keep species present in the data
    sppMerge <- sppMerge[sapply(sppMerge, FUN = function(x) all(x %in% names(speciesLayers)))]

    if (length(sppMerge)) {
      for (i in 1:length(sppMerge)) {
        sumSpecies <- sppMerge[[i]]
        newLayerName <- names(sppMerge)[i]

        fname <- .suffix(file.path(dPath, paste0("KNN", newLayerName, ".tif")), suffix)
        a <- Cache(sumRastersBySpecies,
                   speciesLayers = speciesLayers[sumSpecies],
                   newLayerName = newLayerName,
                   filenameToSave = asPath(fname),
                   ...)
        a <- raster(fname) ## ensure a gets a filename

        ## replace spp rasters by the summed one
        speciesLayers[sumSpecies] <- NULL
        speciesLayers[[newLayerName]] <- a
      }
    }
  }

  ## Rename species layers - note: merged species were renamed already (these can appear as NAs)
  nameChanges <- equivalentName(names(speciesLayers), speciesEquivalency, column = sppEndNamesCol)
  names(speciesLayers)[!is.na(nameChanges)] <- nameChanges[!is.na(nameChanges)]

  ## remove layers that have less data than thresh (i.e. spp absent in study area)
  ## count no. of pixels that have biomass
  layerData <- Cache(sapply, X = speciesLayers, function(x) sum(x[] > 0, na.rm = TRUE))

  ## remove layers that had < thresh pixels with biomass
  belowThresh <- layerData < thresh
  if (any(belowThresh))
    speciesLayers[belowThresh] <- NULL

  ## return stack and updated species names vector
  list(speciesLayers = stack(speciesLayers), sppNameVector = names(speciesLayers))
}

#' Function to sum rasters of species layers
#'
#' @param speciesLayers stack of species layers rasters
#' @param layersToSum names/indices of layers to be summed - optional
#' @param filenameToSave file path to save output raster
#' @param newLayerName name of the output raster layer
#'
#' @export
#' @importFrom raster calc stack writeRaster
sumRastersBySpecies <- function(speciesLayers, layersToSum, filenameToSave, newLayerName) {
  ras_out <- raster::calc(raster::stack(speciesLayers[layersToSum]), sum)
  names(ras_out) <- newLayerName
  writeRaster(ras_out, filename = filenameToSave, datatype = "INT2U", overwrite = TRUE)
  ras_out # Work around for Cache
}

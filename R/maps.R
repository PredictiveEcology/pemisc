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
  flammableTable <- cbind(oldClass, newClass)
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
                          destinationPath = asPath("."),
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
        destinationPath = asPath(destinationPath),
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
#' @param rasterToMatch passed to \code{prepInputs}
#' @param studyArea passed to \code{prepInputs}
#' @param speciesList either a character vector of species names to download,
#'                    or a two-column matrix with the species names to download
#'                    and final names, with column names
#'                    \code{c("speciesNamesRaw", "speciesNamesEnd")}.
#'                    Should two raw species names share the same final name,
#'                    their biomass data will be considered as the "same species".
#' @param thresh the minimum number of pixels where the species must have
#'               \code{biomass > 0} to be considered present in the study area.
#'               Defaults to 1.
#' @param url the source url for the data, passed to \code{prepInputs}
#' @param cachePath path to the cache directory
#' @param ... Additonal arguments
#'
#' @return a list of two elements: \code{speciesLayer}, a raster stack; and
#'         \code{speciesList}, a vector(?) of species names. TODO: verify this
#'
#' @export
#' @importFrom raster ncell
#' @importFrom reproducible Cache
#' @importFrom utils untar
#'
loadkNNSpeciesLayers <- function(dPath, rasterToMatch, studyArea,
                                 speciesList = NULL, thresh = 1, url, cachePath, ...) {
  if (class(speciesList) == "matrix") {
    ## check column names
    if (!setequal(colnames(speciesList), c("speciesNamesRaw", "speciesNamesEnd")))
      stop("names(species) must be c('speciesNamesRaw', 'speciesNamesEnd'),",
           "for raw species names and final species names respectively")
  }

  # Changed by Eliot Oct 20 2018 -- can't start with untar because tar file may not be present
  suffix <- if (basename(cachePath) == "cache") {
    paste0(as.character(ncell(rasterToMatch)), "px")
  } else {
    basename(cachePath)
  }
  suffix <- paste0("_", suffix)

  ## Make sure raw names are compatible with kNN names
  kNNnames <- lapply(strsplit(speciesList[,1], "_"), function(x) {
    x[1] <- substring(x[1], 1, 4)
    x[2] <- paste0(toupper(substring(x[2], 1, 1)), substring(x[2], 2, 3))
    x
  })
  kNNnames <- sapply(kNNnames, function(x) paste(x, collapse = "_"))
  speciesList[, 1] <- kNNnames

  species1 <- Cache(knnLoadFun, url = url, spp = speciesList, #[, "speciesNamesRaw"],
                    #loadFun,
                    dPath = dPath,
                    suffix = suffix,
                    studyArea = studyArea,
                    rasterToMatch = rasterToMatch,
                    userTags = "kNN_SppLoad")

  ## get all kNN species
  if (FALSE) { # TODO: This no longer does all species
    allSpp <- Cache(untar, tarfile = file.path(dPath, "kNN-Species.tar"), list = TRUE)
    allSpp <- allSpp %>%
      grep(".zip", ., value = TRUE) %>%
      sub("_v0.zip", "", .) %>%
      sub(".*Species_", "", .)


    ## check for missing species
    if (any(!speciesList[,1] %in% allSpp)) {
      warning("Some species not present in kNN database./n  Check if this is correct.")
      speciesList <- speciesList[speciesList[, 1] %in% allSpp,]
    }
  }

  names(species1) <- speciesList[, "speciesNamesRaw"]

  ## Sum species that share same final name
  if (any(duplicated(speciesList[, 2]))) {
    dubs <- unique(speciesList[duplicated(speciesList[, 2]), 2]) ## get the duplicated final names

    ## make a list of species that will be summed (those with duplicated final names)
    spp2sum <- lapply(dubs, FUN = function(x) {
      speciesList[speciesList[, 2] %in% x, 1]
    })

    names(spp2sum) <- dubs

    for (i in 1:length(spp2sum)) {
      sumSpecies <- spp2sum[[i]]
      newLayerName <- names(spp2sum)[i]

      fname <- .suffix(file.path(dPath, paste0("KNN", newLayerName, ".tif")), suffix)
      a <- Cache(sumRastersBySpecies,
                 speciesLayers = species1[sumSpecies],
                 newLayerName = newLayerName,
                 filenameToSave = asPath(fname),
                 ...)
      a <- raster(fname) ## ensure a gets a filename

      ## replace spp rasters by the summed one
      species1[sumSpecies] <- NULL
      species1[[newLayerName]] <- a
    }
  }

  ## Rename species layers - note: merged species were renamed already
  nameReplace <- as.matrix(speciesList[, 2])
  rownames(nameReplace) <- speciesList[, 1]

  toReplace <- names(species1)[names(species1) %in% rownames(nameReplace)]
  names(species1)[names(species1) %in% toReplace] <- nameReplace[toReplace, 1]

  ## remove layers that have less data than thresh (i.e. spp absent in study area)
  ## count no. of pixels that have biomass
  layerData <- Cache(sapply, X = species1, function(x) sum(x[] > 0, na.rm = TRUE))

  ## remove layers that had < thresh pixels with biomass
  belowThresh <- layerData < thresh
  if (any(belowThresh))
    species1[belowThresh] <- NULL

  ## return stack and final species matrix
  list(speciesLayers = stack(species1), speciesList = speciesList)
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
  out <- raster::calc(raster::stack(speciesLayers[layersToSum]), sum)
  names(out) <- newLayerName
  writeRaster(out, filename = filenameToSave, datatype = "INT2U", overwrite = TRUE)
  out # Work around for Cache
}

#' Load kNN species data
#'
#' @param speciesListIndex TODO: description needed
#' @param spp TODO: description needed
#' @param suffix TODO: description needed
#' @param url TODO: description needed
#' @param dPath TODO: description needed
#' @param studyArea TODO: description needed
#' @param rasterToMatch TODO: description needed
#'
#' @export
#' @importFrom reproducible .suffix prepInputs
knnLoadFun <- function(speciesListIndex, spp, suffix, url, dPath, studyArea, rasterToMatch) {
  if (is.null(spp)) {
    knownSp <- c(
      "Abie_Ama", "Abie_Bal", "Abie_Gra", "Abie_Las", "Abie_Spp",
      "Acer_Cir", "Acer_Mac", "Acer_Neg", "Acer_Pen", "Acer_Rub", "Acer_Sac", "Acer_Sah", "Acer_Spi", "Acer_Spp", # nolint
      "Alnu_Inc_Rug", "Alnu_Inc_Ten", "Alnu_Inc", "Alnu_Rub", "Alnu_Spp",
      "Arbu_Men",
      "Asim_Tri",
      "Betu_All", "Betu_Pap", "Betu_Pop", "Betu_Spp",
      "Carp_Car",
      "Cary_Cor",
      "Cast_Den",
      "Cham_Noo",
      "Crat_Spp",
      "Fagu_Gra",
      "Frax_Ame", "Frax_Nig", "Frax_Pen_Sub", "Frax_Pen", "Frax_Spp",
      "Generic_BroadLeaf_Spp",
      "Generic_NeedleLeaf_Spp",
      "Gled_Tri",
      "Jugl_Cin", "Jugl_Nig",
      "Juni_Vir",
      "Lari_Kae", "Lari_Lar", "Lari_Lya", "Lari_Occ", "Lari_Spp",
      "Malu_Fus", "Malu_Spp",
      "Ostr_Vir",
      "Pice_Abi", "Pice_Eng_Gla", "Pice_Eng", "Pice_Gla", "Pice_Mar", "Pice_Rub", "Pice_Sit", "Pice_Spp", # nolint
      "Pinu_Alb", "Pinu_Ban", "Pinu_Con_Lat", "Pinu_Con", "Pinu_Fle", "Pinu_Mon",
      "Pinu_Pon", "Pinu_Res", "Pinu_Rig", "Pinu_Spp", "Pinu_Str", "Pinu_Syl",
      "Plat_Occ",
      "Popu_Bal", "Popu_Del", "Popu_Gra", "Popu_Spp", "Popu_Tre", "Popu_Tri",
      "Prun_Pen", "Prun_Ser", "Prun_Vir",
      "Pseu_Men_Gla", "Pseu_Men_Men", "Pseu_Men",
      "Quer_Alb", "Quer_Bic", "Quer_Gar", "Quer_Mac", "Quer_Rub",
      "Robi_Pse",
      "Sali_Beb", "Sali_Nig", "Sali_Spp",
      "Sass_Alb",
      "Sorb_Ame", "Sorb_Dec", "Sorb_Spp",
      "Thuj_Occ", "Thuj_Pli", "Thuj_Spp",
      "Tili_Ame",
      "Tsug_Can", "Tsug_Het", "Tsug_Mer_Het", "Tsug_Mer", "Tsug_Spp",
      "Ulmu_Ame", "Ulmu_Rub", "Ulmu_Spp", "Ulmu_Tho"
    )
    stop("This loadFun has not been tested for all species. ",
         "Please specify the actual species desired by name. ",
         "Known species are:\n", paste(knownSp, collapse = "\n"))
  }

  archive <- asPath("kNN-Species.tar")
  ## check if species is a vector/matrix
  if (is.null(spp)) {
    ## set to NULL so prepInputs extracts all of them
    targetFile <- NULL

    # just get tar file, no crop/reproject etc. Too many
    tarFile <- prepInputs(
      targetFile = targetFile,
      url = url,
      archive = archive,
      destinationPath = asPath(dPath),
      fun = "raster::raster"#),
      #studyArea = studyArea,
      #rasterToMatch = rasterToMatch,
      #method = "bilinear",
      #datatype = "INT2U",
      #filename2 = postProcessedFilename
    )

    ## make a matrix of raw and final species names
    spp <-  matrix(data = rep(spp, 2), nrow = length(spp), ncol = 2, byrow = FALSE)
    colnames(spp) <- c("speciesNamesRaw", "speciesNamesEnd")
  } else if (class(spp) == "matrix") {
    ## check column names
    if (!setequal(colnames(spp), c("speciesNamesRaw", "speciesNamesEnd")))
      stop("names(species) must be c('speciesNamesRaw', 'speciesNamesEnd'), ",
           "for raw species names and final species names respectively.")

    targetFiles <- paste0("NFI_MODIS250m_kNN_Species_", spp[, "speciesNamesRaw"], "_v0.tif")
    names(targetFiles) <- targetFiles
    archives <- cbind(archive1 = archive,
                      archive2 = paste0("NFI_MODIS250m_kNN_Species_", spp[, "speciesNamesRaw"], "_v0.zip"))
    archives <- split(archives, archives[, "archive2"])
  } else stop("species must be a character vector or a two-column matrix")

  postProcessedFilenames <- .suffix(targetFiles, suffix = suffix)

  species1 <- Map(targetFile = targetFiles, archive = archives,
                  filename2 = postProcessedFilenames,
                  MoreArgs = list(url = url,
                                  destinationPath = asPath(dPath),
                                  fun = "raster::raster",
                                  studyArea = studyArea,
                                  rasterToMatch = rasterToMatch,
                                  method = "bilinear",
                                  datatype = "INT2U"
                  ),
                  prepInputs)

  names(species1) <- spp[, "speciesNamesRaw"]
  return(species1)
}

#' Overlay layers within raster stacks
#'
#' TODO: description needed
#'
#' @param highQualityStack      TODO: description needed
#' @param lowQualityStack       TODO: description needed
#' @param speciesList           TODO: description needed
#' @param outputFilenameSuffix  TODO: description needed
#' @param destinationPath       TODO: description needed
#'
#' @export
#' @importFrom gdalUtils gdalwarp
#' @importFrom quickPlot layerNames
#' @importFrom raster compareRaster crs extent filename ncell projectExtent
#' @importFrom raster raster res writeRaster xmax xmin ymax ymin
overlayStacks <- function(highQualityStack, lowQualityStack, speciesList = NULL,
                          outputFilenameSuffix = "overlay", destinationPath) {

  if (!is.null(speciesList)) {
    ## TODO: need better (non-manual) way to match layerNames between stacks!
    names(highQualityStack) <- speciesList
  }

  for (sp in layerNames(highQualityStack)) {
    hqLarger <- ncell(lowQualityStack) * prod(res(lowQualityStack)) <
      ncell(highQualityStack) * prod(res(highQualityStack))

    if (sp %in% layerNames(lowQualityStack)) {
      if (!(all(
        isTRUE(all.equal(extent(lowQualityStack), extent(highQualityStack))),
        isTRUE(all.equal(crs(lowQualityStack), crs(highQualityStack))),
        isTRUE(all.equal(res(lowQualityStack), res(highQualityStack)))))) {
        message("  ", sp, " extents, or resolution, or projection did not match; ",
                "using gdalwarp to make them overlap")
        LQRastName <- basename(tempfile(fileext = ".tif"))
        if (!nzchar(filename(lowQualityStack[[sp]]))) {
          LQCurName <- basename(tempfile(fileext = ".tif"))
          lowQualityStack[[sp]][] <- as.integer(lowQualityStack[[sp]][])
          lowQualityStack[[sp]] <- writeRaster(lowQualityStack[[sp]],
                                               filename = LQCurName,
                                               datatype = "INT2U")
        }

        LQRastInHQcrs <- projectExtent(lowQualityStack, crs = crs(highQualityStack))
        # project LQ raster into HQ dimensions
        gdalwarp(overwrite = TRUE,
                 dstalpha = TRUE,
                 s_srs = as.character(crs(lowQualityStack[[sp]])),
                 t_srs = as.character(crs(highQualityStack[[sp]])),
                 multi = TRUE, of = "GTiff",
                 tr = res(highQualityStack),
                 te = c(xmin(LQRastInHQcrs), ymin(LQRastInHQcrs),
                        xmax(LQRastInHQcrs), ymax(LQRastInHQcrs)),
                 filename(lowQualityStack[[sp]]), ot = "Byte",
                 LQRastName)

        LQRast <- raster(LQRastName)
        LQRast[] <- LQRast[]
        unlink(LQRastName)

        try(unlink(LQCurName), silent = TRUE)

        if (hqLarger) {
          tmpHQName <- basename(tempfile(fileext = ".tif"))

          gdalwarp(overwrite = TRUE,
                   dstalpha = TRUE,
                   s_srs = as.character(crs(highQualityStack[[sp]])),
                   t_srs = as.character(crs(highQualityStack[[sp]])),
                   multi = TRUE, of = "GTiff",
                   tr = res(highQualityStack),
                   te = c(xmin(LQRastInHQcrs), ymin(LQRastInHQcrs),
                          xmax(LQRastInHQcrs), ymax(LQRastInHQcrs)),
                   filename(highQualityStack[[sp]]), ot = "Byte", tmpHQName)
          HQRast <- raster(tmpHQName)
          HQRast[] <- HQRast[]
          HQRast[HQRast[] == 255] <- NA_integer_
          unlink(tmpHQName)
        } else {
          HQRast <- highQualityStack[[sp]]
        }
      } else {
        LQRast <- lowQualityStack[[sp]]
        HQRast <- highQualityStack[[sp]]
      }
      message("  Writing new, overlaid ", sp, " raster to disk.")
      if (!compareRaster(LQRast, HQRast))
        stop("Stacks not identical, something is wrong with overlayStack function.")

      nas <- is.na(HQRast[])
      HQRast[nas] <- LQRast[][nas]
      HQRast <- writeRaster(HQRast, datatype = "INT2U",
                            filename = file.path(
                              destinationPath,
                              paste0(sp, "_", outputFilenameSuffix, ".tif")
                            ),
                            overwrite = TRUE)
      names(HQRast) <- sp

      if (!exists("HQStack")) HQStack <- stack(HQRast) else HQStack[[sp]] <- HQRast
    }
  }
  HQStack
}

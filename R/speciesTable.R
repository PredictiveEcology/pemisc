if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(":=", ".SD", "col1", "growthcurve",
                           "leafLignin", "leaflongevity", "mortalityshape",
                           "seeddistance_eff", "seeddistance_max",
                           "species", "species1", "species2", "wooddecayrate"))
}

#' Download and prepare a species traits table for use with LBMR module
#'
#' TODO: add detailed description
#'
#' @note This one is tailored to Canadian forests (?)
#'
#' @param url If NULL (the default), uses one from D. Cyr's LANDIS-II files:
#' \url{https://github.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv}).
#'
#' @param dPath The destination path.
#'
#' @param cacheTags User tags to pass to \code{Cache}.
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom magrittr %>%
#' @importFrom reproducible asPath Cache prepInputs
#' @rdname speciesTable
getSpeciesTable <- function(url = NULL, dPath = tempdir(), cacheTags = NULL) {
  if (is.null(url))
    url <- paste0("https://raw.githubusercontent.com/",
                  "dcyr/LANDIS-II_IA_generalUseFiles/",
                  "master/speciesTraits.csv")

  speciesTable <- Cache(prepInputs, "speciesTraits.csv",
                        destinationPath = asPath(dPath),
                        url = url,
                        fun = "utils::read.csv",
                        header = TRUE, stringsAsFactors = FALSE,
                        userTags = c(cacheTags, "speciesTable")) %>%
    data.table()

  return(speciesTable)
}

#' @param speciesTable  A raw species traits table
#'
#' @param speciesList   TODO: description needed
#'
#' @param speciesLayers TODO: description needed
#'
#' @return A \code{data.table} with columns ... TODO
#'
#' @export
#' @rdname speciesTable
prepSpeciesTable <- function(speciesTable, speciesList, speciesLayers) {
  names(speciesTable) <- c(
    "species",
    "Area",
    "longevity",
    "sexualmature",
    "shadetolerance",
    "firetolerance",
    "seeddistance_eff",
    "seeddistance_max",
    "resproutprob",
    "resproutage_min",
    "resproutage_max",
    "postfireregen",
    "leaflongevity",
    "wooddecayrate",
    "mortalityshape",
    "growthcurve",
    "leafLignin",
    "hardsoft"
  )
  speciesTable[, ':='(Area = NULL, hardsoft = NULL)] ## hardsoft used in fire model
  speciesTable$species1 <- as.character(substring(speciesTable$species, 1, 4))
  speciesTable$species2 <- as.character(substring(speciesTable$species, 6,
                                                  nchar(as.character(speciesTable$species))))
  speciesTable[, ':='(species = paste(
    as.character(substring(species1, 1, 1)),
    tolower(as.character(substring(species1, 2, nchar(species1)))),
    "_", as.character(substring(species2, 1, 1)),
    tolower(as.character(substring(species2, 2, nchar(species2)))),
    sep = ""))]

  speciesTable$species <- toSentenceCase(speciesTable$species)
  speciesTable[species == "Pinu_con.con", species := "Pinu_con"]
  speciesTable[species == "Pinu_con.lat", species := "Pinu_con"]
  speciesTable[species == "Betu_all", species := "Betu_sp"]

  ## convert species names to match user-input list
  rownames(speciesList) <- sapply(strsplit(speciesList[, 1], "_"), function(x) {
    x[1] <- substring(x[1], 1, 4)
    x[2] <-  substring(x[2], 1, 3)
    paste(x, collapse = "_")
  })

  ## replace eventual "spp" and "all" by sp (currently used instead of spp)
  rownames(speciesList) <- sub("_spp*", "_sp", rownames(speciesList))
  rownames(speciesList) <- sub("_all", "_sp", rownames(speciesList))

  ## match rownames to speciesTable$species
  rownames(speciesList) <- toSentenceCase(rownames(speciesList))

  ## find matching names to replace in speciesTable
  matchNames <- speciesTable[species %in% rownames(speciesList), species]
  speciesTable[species %in% rownames(speciesList), species := speciesList[matchNames, 2]]

  ## filter table to existing species layers
  speciesTable <- speciesTable[species %in% names(speciesLayers)]

  ## Take the smallest values of every column, within species, because it is northern boreal forest
  speciesTable <- speciesTable[species %in% names(speciesLayers), ][
    , ':='(species1 = NULL, species2 = NULL)] %>%
    .[, lapply(.SD, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else x[1]), by = "species"]

  return(speciesTable)
}

#' Download and prepare a species traits table for use with \code{LandR_BiomassGMOrig} module
#'
#' TODO: add detailed description
#'
#' @note This one is tailored to Canadian forests (?)
#'
#' @param url If NULL (the default), uses one from the LANDIS-II project:
#' \url{https://github.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"}).
#'
#' @param dPath The destination path.
#'
#' @param cacheTags User tags to pass to \code{Cache}.
#'
#' @export
#' @importFrom data.table data.table setcolorder
#' @importFrom reproducible asPath Cache prepInputs
#'
#' @return A \code{data.table} with columns ... TODO
#'
#' @export
#' @rdname prepInputsSpecies
prepInputsSpecies <- function(url, dPath, cacheTags) {
  mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL

  maxcol <- 13#max(count.fields(file.path(dPath, "species.txt"), sep = ""))
  species <- Cache(prepInputs,
                   url = url,
                   targetFile = "species.txt",
                   destinationPath = dPath,
                   fun = "utils::read.table",
                   fill = TRUE, row.names = NULL, #purge = 7,
                   sep = "",
                   header = FALSE,
                   blank.lines.skip = TRUE,
                   col.names = c(paste("col",1:maxcol, sep = "")),
                   stringsAsFactors = FALSE,
                   overwrite = TRUE)
  species <- data.table(species[, 1:11])
  species <- species[col1!= "LandisData",]
  species <- species[col1!= ">>",]
  colNames <- c("species", "longevity", "sexualmature", "shadetolerance",
                "firetolerance", "seeddistance_eff", "seeddistance_max",
                "resproutprob", "resproutage_min", "resproutage_max",
                "postfireregen")
  names(species) <- colNames
  species[, ':='(seeddistance_eff = gsub(",", "", seeddistance_eff),
                 seeddistance_max = gsub(",", "", seeddistance_max))]
  # change all columns to integer
  species <- species[, lapply(.SD, as.integer), .SDcols = names(species)[-c(1, NCOL(species))],
                     by = "species,postfireregen"]
  setcolorder(species, colNames)

  # get additional species traits
  speciesAddon <- mainInput
  startRow <- which(speciesAddon$col1 == "SpeciesParameters")
  speciesAddon <- speciesAddon[(startRow + 1):(startRow + nrow(species)), 1:6, with = FALSE]
  names(speciesAddon) <- c("species", "leaflongevity", "wooddecayrate",
                           "mortalityshape", "growthcurve", "leafLignin")
  speciesAddon[, ':='(leaflongevity = as.numeric(leaflongevity),
                      wooddecayrate = as.numeric(wooddecayrate),
                      mortalityshape = as.numeric(mortalityshape),
                      growthcurve = as.numeric(growthcurve),
                      leafLignin = as.numeric(leafLignin))]

  species <- setkey(species, species)[setkey(speciesAddon, species), nomatch = 0]

  ## TODO: use species equivalency table here
  ## rename species for compatibility across modules (Genu_spe)
  species$species1 <- as.character(substring(species$species, 1, 4))
  species$species2 <- as.character(substring(species$species, 5, 7))
  species[, ':='(species = paste0(toupper(substring(species1, 1, 1)),
                                  substring(species1, 2, 4), "_",
                                  species2))]

  species[, ':='(species1 = NULL, species2 = NULL)]

  return(species)
}

#' @export
#' @rdname prepInputsSpecies
prepInputsMainInput <- function(url = NULL, dPath, cacheTags) {
  if (is.null(url))
    url <- paste0("https://raw.githubusercontent.com/LANDIS-II-Foundation/",
                  "Extensions-Succession/master/biomass-succession-archive/",
                  "trunk/tests/v6.0-2.0/biomass-succession_test.txt")

  maxcol <- 7L
  mainInput <- Cache(prepInputs,
                     url = url,
                     targetFile = "biomass-succession_test.txt",
                     destinationPath = dPath,
                     userTags = cacheTags,
                     fun = "utils::read.table",
                     fill = TRUE,  #purge = 7,
                     sep = "",
                     header = FALSE,
                     col.names = c(paste("col", 1:maxcol, sep = "")),
                     blank.lines.skip = TRUE,
                     stringsAsFactors = FALSE)

  mainInput <- data.table(mainInput)
  mainInput <- mainInput[col1 != ">>",]

  return(mainInput)
}

if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(":=", ".SD", "species", "species1", "species2"))
}

#' Download species traits table for use with LBMR module
#'
#' @param dPath The destination path.
#' @param cacheTags User tags to pass to \code{Cache}.
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom magrittr %>%
#' @importFrom reproducible Cache prepInputs
#' @rdname speciesTable
getSpeciesTable <- function(dPath = tempdir(), cacheTags = NULL) {
  speciesTableURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv" # nolint
  speciesTable <- Cache(prepInputs, "speciesTraits.csv",
                        destinationPath = dPath,
                        url = speciesTableURL,
                        fun = "utils::read.csv",
                        header = TRUE, stringsAsFactors = FALSE,
                        userTags = c(cacheTags, "speciesTable")) %>%
    data.table()

  return(speciesTable)
}

#' Prepare a species traits table for use with LBMR module
#'
#' TODO: add detailed description
#'
#' @param speciesTable  A raw species traits table from the LANDIS-II project
#' (e.g., \url{"https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/speciesTraits.csv"}).
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

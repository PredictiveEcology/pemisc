if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", ":="))
}

#' Generate vegetation type map
#'
#' @param species        A \code{data.table} of stuff
#' @param cohortdata     A different \code{data.table} of stuff
#' @param pixelGroupMap  A \code{raster}
#' @param vegLeadingProportion Numeric between 0-1.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table data.table setkey
#' @importFrom raster getValues projection projection<- setValues
#' @importFrom SpaDES.tools rasterizeReduced
vegTypeMapGenerator <- function(species, cohortdata, pixelGroupMap, vegLeadingProportion) {
  species[species == "Pinu_ban" | species == "Pinu_con" | species == "Pinu_sp",
          speciesGroup := "PINU"]
  species[species == "Betu_pap" | species == "Popu_bal" | species == "Popu_tre" |
            species == "Lari_lar", speciesGroup := "DECI"]
  species[species == "Pice_mar", speciesGroup := "PICE_MAR"]
  species[species == "Pice_gla", speciesGroup := "PICE_GLA"]
  species[species == "Abie_sp", speciesGroup := "ABIE"]

  shortcohortdata <- setkey(cohortdata, speciesCode)[setkey(species[, .(speciesCode, speciesGroup)],
                                                            speciesCode), nomatch = 0]
  shortcohortdata[, totalB := sum(B, na.rm = TRUE), by = pixelGroup]
  shortcohortdata <- shortcohortdata[, .(speciesGroupB = sum(B, na.rm = TRUE),
                                         totalB = mean(totalB, na.rm = TRUE)),
                                     by = c("pixelGroup", "speciesGroup")]
  shortcohortdata[, speciesProportion := speciesGroupB / totalB]

  speciesLeading <- NULL
  Factor <- NULL
  ID <- NULL
  pixelGroup <- NULL
  speciesProportion <- NULL
  speciesGroup <- NULL
  speciesCode <- NULL
  totalB <- NULL
  B <- NULL
  speciesGroupB <- NULL

  shortcohortdata[speciesGroup == "PINU" & speciesProportion > vegLeadingProportion,
                  speciesLeading := 1] # pine leading
  shortcohortdata[speciesGroup == "DECI" & speciesProportion > vegLeadingProportion,
                  speciesLeading := 2] # deciduous leading
  shortcohortdata[speciesGroup == "PICE_MAR" & speciesProportion > vegLeadingProportion,
                  speciesLeading := 3] # black spruce leading
  shortcohortdata[speciesGroup == "PICE_GLA" & speciesProportion > vegLeadingProportion,
                  speciesLeading := 4] # white spruce leading
  shortcohortdata[is.na(speciesLeading), speciesLeading := 0]
  shortcohortdata[, speciesLeading := max(speciesLeading, na.rm = TRUE), by = pixelGroup]
  shortcohortdata <- unique(shortcohortdata[, .(pixelGroup, speciesLeading)], by = "pixelGroup")
  shortcohortdata[speciesLeading == 0, speciesLeading := 5] # 5 is mixed forests
  attritable <- data.table(ID = sort(unique(shortcohortdata$speciesLeading)))
  attritable[ID == 1, Factor := "Pine leading"]
  attritable[ID == 2, Factor := "Deciduous leading"]
  attritable[ID == 3, Factor := "Black spruce leading"]
  attritable[ID == 4, Factor := "White spruce leading"]
  attritable[ID == 5, Factor := "Mixed"]
  vegTypeMap <- rasterizeReduced(shortcohortdata, pixelGroupMap, "speciesLeading", "pixelGroup")
  vegTypeMap <- setValues(vegTypeMap, as.integer(getValues(vegTypeMap)))
  levels(vegTypeMap) <- as.data.frame(attritable)
  projection(vegTypeMap) <- projection(pixelGroupMap)
  vegTypeMap
}

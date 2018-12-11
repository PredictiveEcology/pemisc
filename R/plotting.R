#' Summary plots of leading vegetation types
#'
#' Create raster of leading vegetation types and \code{Plot} a bar chart summary
#' and a vegetation type map.
#'
#' @param speciesStack A \code{RasterStack} of percent-cover-by-species layers.
#'
#' @param vtm An optional vegetation type map (\code{RasterLayer}).
#'            If not supplied, will be produced internally by \code{makeVegTypeMap}.
#'
#' @param vegLeadingProportion The minimum proportion cover required to consider
#'                             a species to be the "leading" one. Default 0.8.
#'
#' @param sppEquiv a species equivalency table TODO: description needed
#'
#' @param sppEquivCol the column name to use from \code{sppEquiv}.
#'
#' @param colors Named vector of colour codes, named using species names.
#'
#' @param title The title to use for the generated plots.
#'
#' @author Eilot McIntire
#' @export
#' @importFrom data.table data.table
#' @importFrom ggplot2 aes element_blank element_text geom_bar ggplot scale_fill_manual theme
#' @importFrom quickPlot Plot setColors<-
#' @importFrom raster factorValues maxValue minValue
#' @importFrom reproducible Cache
plotVTM <- function(speciesStack = NULL, vtm = NULL, vegLeadingProportion = 0.8,
                    sppEquiv, sppEquivCol, colors, title = "Leading vegetation types") {
  if (is.null(vtm)) {
    if (!is.null(speciesStack))
      vtm <- Cache(makeVegTypeMap, speciesStack, vegLeadingProportion, mixed = TRUE)
    else
      stop("plotVTM requires either a speciesStack of percent cover or a vegetation type map (vtm).")
  }

  ## the ones we want
  sppEquiv <- sppEquiv[!is.na(sppEquiv[[sppEquivCol]]),]

  ## plot initial types bar chart
  facVals <- factorValues2(vtm, vtm[], att = "Species", na.rm = TRUE)
  df <- data.table(species = as.character(facVals), stringsAsFactors = FALSE)
  df <- df[!is.na(df$species)]

  colorsEN <- equivalentName(names(colors), sppEquiv, "EN_generic_short")
  speciesEN <- equivalentName(df$species, sppEquiv, "EN_generic_short")
  if (all(na.omit(speciesEN) %in% colorsEN) ){
    colDT <- data.table(cols = colors, species = colorsEN)

    hasMixed <- isTRUE("Mixed" %in% unique(df$species))
    if (hasMixed) {
      mixedString <- "Mixed"
      whMixed <- which(df$species == mixedString)
      whMixedColors <- which(names(colors) == mixedString)
      colDT[whMixedColors, species := mixedString]

    }

    df$species <- speciesEN

    if (hasMixed)
      df[whMixed, species := mixedString]


    df <- colDT[df, on = "species"] # merge color and species

  } else {
    stop("Species names of 'colors' must match those in 'speciesStack'.")
  }


  cols2 <- df$cols
  names(cols2) <- df$species # This makes colours match the species

  initialLeadingPlot <- ggplot(data = df, aes(species, fill = species)) +
    scale_fill_manual(values = cols2) +
    geom_bar(position = "stack") +
    theme(legend.text = element_text(size = 6), legend.title = element_blank(),
          axis.text = element_text(size = 6))

  Plot(initialLeadingPlot, title = title)

  browser()
  ## plot inital types raster
  vtmTypes <- factorValues(vtm, seq(minValue(vtm), maxValue(vtm)), att = "Species")[[1]]
  vtmCols <- equivalentName(vtmTypes, df = sppEquiv, "cols")
  setColors(vtm, vtmTypes) <- vtmCols

  Plot(vtm, title = title)
}

#' Create species color vector from a sppEquiv table
#'
#' Create species color vector from a sppEquiv table
#'
#' @param sppEquiv A species equivalency table, e.g., \code{data("sppEquivalencies_CA")}.
#' @param sppEquivCol The name of the column to get names from.
#' @param newVals An optional character vector of extra names to use, e.g., "Mixed".
#' @param palette An RColorBrewer palette, e.g., "Accent".
#'                Can get RColorBrewer palette names from
#'                \code{rownames(RColorBrewer::brewer.pal.info)}.
#'
#' @return A named vector of color codes, where the names are the species names
#' plus any extra names passed with \code{newVals}.
#'
#' @export
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
sppColors <- function(sppEquiv, sppEquivCol, newVals, palette) {
  sppColorNames <- c(na.omit(unique(sppEquiv[[sppEquivCol]])), newVals)

  sppColors <- NULL
  sppColors <- if (is.character(palette))
    if (palette %in% rownames(RColorBrewer::brewer.pal.info))
      RColorBrewer::brewer.pal(length(sppColorNames), palette)

  if (is.null(sppColors))
    stop("Currently palette must be one of the RColorBrewer::brewer.pal names")
  names(sppColors) <- sppColorNames
  sppColors
}

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
#' @param title The title te use for the generated plots.
#'
#' @author Eilot McIntire
#' @export
#' @importFrom data.table data.table
#' @importFrom ggplot2 aes element_blank element_text geom_bar ggplot scale_fill_manual theme
#' @importFrom quickPlot Plot setColors<-
#' @importFrom raster factorValues maxValue minValue
#' @importFrom reproducible Cache
plotVTM <- function(speciesStack = NULL, vtm = NULL, vegLeadingProportion = 0.8,
                    sppEquiv, sppEquivCol, title = "Leading vegetation types") {
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
  df$species <- equivalentName(df$species, sppEquiv, "EN_generic_short")
  df$cols <- equivalentName(df$species, sppEquiv, "cols")

  cols2 <- df$cols
  names(cols2) <- df$species
  initialLeadingPlot <- ggplot(data = df, aes(species, fill = species)) +
    scale_fill_manual(values = cols2) +
    geom_bar(position = "stack") +
    theme(legend.text = element_text(size = 6), legend.title = element_blank(),
          axis.text = element_text(size = 6))

  Plot(initialLeadingPlot, title = title)

  ## plot inital types raster
  vtmTypes <- factorValues(vtm, seq(minValue(vtm), maxValue(vtm)), att = "Species")[[1]]
  vtmCols <- equivalentName(vtmTypes, df = sppEquiv, "cols")
  setColors(vtm, vtmTypes) <- vtmCols

  Plot(vtm, title = title)
}

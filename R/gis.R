#' Create a .prj file
#'
#' In cases where a shapefile is missing its associated .prj file.
#'
#' @param shpFile The filename of a shapefile to add .prj
#' @param prjFile The filename of a
createPrjFile <- function(shpFile, prjFile) {
  basenameWithoutExt <- tools::file_path_sans_ext(shpFile)
  basenameWithoutExt <- basenameWithoutExt[-length(basenameWithoutExt)]
  prjFile <- paste0(basenameWithoutExt, ".prj")
  if (!file.exists(prjFile)) {
    download.file("http://spatialreference.org/ref/epsg/nad83-utm-zone-11n/prj/",
                  destfile = prjFile)
  }
}

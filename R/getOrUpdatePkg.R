#' Get or update packages
#'
#' @param p character string denoting a package name
#' @param minVer character string denoting the minimum package version
#'
#' @returns invoked for side effect of installing packages
#'
#' @importFrom utils install.packages packageVersion
getOrUpdatePkg <- function(p, minVer = "0") {
  repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
  if (length(p) != length(minVer)) stop("minVer must be as long as p")
  p <- Map(pp = p, mv = minVer, function(pp, mv) {
    if (!isFALSE(try(packageVersion(pp) < mv, silent = TRUE))) {
      pp
    } else {
      NULL
    }
  }
  )
  if (length(unlist(p))) {
    install.packages(unlist(p), repos = repo)
  }
}

#' Build the pkg dependency graph
#'
#' Uses \pkg{igraph} and `reproducible::pkgDep`.
#'
#' @param pkgs A character vector of package names. Default is
#' `c("LandR", "pemisc", "map", "SpaDES", "SpaDES.tools", "SpaDES.core", "SpaDES.addins", "SpaDES.shiny", "reproducible", "quickPlot")`
#'
#' @param plot.it Logical. If `TRUE`, it will plot the `igraph`.
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom graphics plot
#' @importFrom reproducible pkgDep
#' @return
#' A list of 2: `dt` a data.table of the dependencies, and `dtGraph`
#' an `igraph` object that can be plotted with `plot()`
pkgDepsGraph <- function(pkgs = c("LandR", "pemisc", "map", "SpaDES",
                                  "SpaDES.tools", "SpaDES.core",
                                  "SpaDES.addins", "SpaDES.shiny",
                                  "reproducible", "quickPlot"),
                         plot.it = TRUE) {
  if (reproducible::.requireNamespace("igraph")) {
    dt <- lapply(pkgs, function(pkg) {
      deps <- pkgs[pkgs %in% pkgDep(pkg)[[1]]]
      if (NROW(deps))
        data.table(pkg = pkg,
                                depends = deps)
    })
    dt <- rbindlist(dt);
    dtGraph <- igraph::graph_from_data_frame(dt);
    if (isTRUE(plot.it))
      plot(dtGraph)
    return(list(dt = dt, dtGraph = dtGraph))
  }
}

#' Check whether a package is one installed from GitHub
#'
#' Determines whether a string that may correspond to a package name
#' (e.g., `repo/package@branch`), could be a package installed from GitHub.
#' This is determined solely by the presence of a `/` in the string.
#' See example below.
#'
#' @param x character vector of package names
#'
#' @return a named logical vector
#'
#' @export
#'
#' @examples
#' pkgs <- c("dplyr", "PredictiveEcology/pemisc", "PredictiveEcology/SpaDES.core@development")
#' isGitHubPkg(pkgs) ## FALSE TRUE TRUE
isGitHubPkg <- Vectorize(function(x) {
  if (length(strsplit(x, "/")[[1]]) == 1) FALSE else TRUE
})

#' Get the package name from a GitHub `repo/package@branch` string
#'
#' @param x character vector of package names
#'
#' @return a named character vector
#'
#' @export
#'
#' @examples
#' pkgs <- c("dplyr", "PredictiveEcology/pemisc", "PredictiveEcology/SpaDES.core@development")
#' ghPkgName(pkgs) ## "dplyr" "pemisc" "SpaDES.core"
ghPkgName <- Vectorize(function(x) {
  y <- strsplit(x, "/")[[1]]

  if (length(y) == 1) {
    repo <- NULL
    pkg <- y
    branch <- NULL
  } else {
    z <- strsplit(y[2], "@")[[1]]

    repo <- y[1]
    pkg <- z[1]
    branch <- z[2]
    if (is.na(branch)) branch <- "master"
  }

  return(pkg)
})

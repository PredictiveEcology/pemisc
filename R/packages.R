#' Build the pkg dependency graph
#'
#' Uses \code{igraph} and \code{reproducible::pkgDep}.
#'
#' @param pkgs A character vector of package names. Default is
#' \code{c("LandR", "pemisc", "map", "SpaDES", "SpaDES.tools", "SpaDES.core", "SpaDES.addins", "SpaDES.shiny", "reproducible", "quickPlot")}
#'
#' @param plot.it Logical. If \code{TRUE}, it will plot the igraph
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom graphics plot
#' @importFrom igraph graph_from_data_frame
#' @importFrom reproducible pkgDep
#' @return
#' A list of 2: \code{dt} a data.table of the dependencies, and \code{dtGraph}
#' an igraph object that can be plotted with \code{plot()}
pkgDepsGraph <- function(pkgs = c("LandR", "pemisc", "map", "SpaDES",
                                  "SpaDES.tools", "SpaDES.core",
                                  "SpaDES.addins", "SpaDES.shiny",
                                  "reproducible", "quickPlot"),
                         plot.it = TRUE) {

  dt <- lapply(pkgs, function(pkg) {
    deps <- pkgs[pkgs %in% pkgDep(pkg)[[1]]]
    if (NROW(deps))
      data.table(pkg = pkg,
                              depends = deps)
  })
  dt <- rbindlist(dt);
  dtGraph <- graph_from_data_frame(dt);
  if (isTRUE(plot.it))
    plot(dtGraph)
  return(list(dt = dt, dtGraph = dtGraph))
}

#' Check whether a package is one installed from GitHub
#'
#' Determines whether a string that may correspond to a package name
#' (e.g., \code{repo/package@branch}), could be a package installed from GitHub.
#' This is determined solely by the presence of a \code{/} in the string.
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

#' Get the package name from a GitHub \code{repo/package@branch} string
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

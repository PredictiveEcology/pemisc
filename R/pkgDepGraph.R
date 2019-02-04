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

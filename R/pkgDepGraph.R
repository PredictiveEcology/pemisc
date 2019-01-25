#' Build the pkg dependency graph
#'
#' Uses \code{igraph} and \code{reproducible::pkgDep}
#' @importFrom data.table data.table rbindlist
#' @importFrom reproducible pkgDep
#' @importFrom igraph graph_from_data_frame
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

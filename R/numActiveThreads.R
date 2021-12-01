#' Count number of active threads
#'
#' This uses \code{ps -ef} so only works on unix-alikes. It will search
#' for the percent CPU use and select only those above 40
#' @param pattern Character string that will be matched to the \code{ps} call
#' @param minCPU A numeric indicating what percent is the minimum to be considered
#'   "active"
#' @return
#' A numeric of the number of active threads that match the pattern
#' @author Eliot McIntire
#' @export
#' @examples
#' \dontrun{
#'  ## Determine how many threads are used in each remote machine in a cluster
#'   cores = "localhost" # put other machine names here
#'   uniqueCores <- unique(cores)
#'   cl <- future::makeClusterPSOCK(uniqueCores, revtunnel = TRUE)
#'   clusterExport(cl, "numActiveThreads")
#'   out <- clusterEvalQ(cl, {
#'     numActiveThreads()
#'   })
#'   names(out) <- uniqueCores
#'   unlist(out)
#'   stopCluster(cl)
#'
#' }
numActiveThreads <- function(pattern = "--slave", minCPU = 50) {
  if (!identical(.Platform$OS.type, "windows")) {
    a0 <- system("ps -ef", intern = TRUE)[-1]
    a4 <- grep(pattern, a0, value = TRUE)
    a5 <- gsub("^.*[[:digit:]]* [[:digit:]]* ([[:digit:]]{1,3}) .*$", "\\1", a4)
    sum(as.numeric(a5) > minCPU)
  } else {
    message("Does not work on Windows")
  }
}

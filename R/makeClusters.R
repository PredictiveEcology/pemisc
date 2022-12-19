#' Create IP addresses for network cluster
#'
#' `makeIpsForNetworkCluster` is a simple wrapper around `makeIps`.
#'
#' @param ipStart Network address prefix (i.e., the first, second, and third triplets of the IP address)
#' @param ipEnd Host IP address identifier (i.e., the final triplet of the IP address)
#' @param availableCores the number of available threads on each machine.
#' @param availableRAM the available RAM on each machine in GB
#' @param nProcess the number of processes
#' @param proc one of `"cores"` or `"ram"`, describing the limiting factor of the
#'             cluster computations
#' @param internalProcesses DESCRIPTION NEEDED
#' @param sizeGbEachProcess the size in GB of each process
#' @param localHostEndIp the address in `ipEnd` corresponding to local host
#'
#' @return A vector of IP addresses associated with each machine in the network cluster.
#'
#' @export
#' @rdname makeIps
makeIpsForNetworkCluster <- function(ipStart = "10.20.0",
                                     ipEnd = c(68, 97, 189, 213, 220, 58, 106, 184, 217),
                                     availableCores = c(50, 50, 50, 50, 50, 50, 23, 23, 23),
                                     availableRAM = c(950, 500, 500, 500, 500, 500, 245, 245, 245),
                                     nProcess = 8,
                                     proc = "cores",
                                     internalProcesses = 10,
                                     sizeGbEachProcess = 35,
                                     localHostEndIp = 68) {
  proc <- tolower(proc)

  machines <- data.frame(
    ipEnd = ipEnd,
    availableCores = availableCores,
    availableRAM = availableRAM,
    stringsAsFactors = FALSE
  )

  nProcess <- ifelse(proc == "cores", nProcess * internalProcesses, nProcess * sizeGbEachProcess)
  IPs <- makeIps(machines = machines,
                 ipStart = ipStart,
                 nProcess = nProcess,
                 proc = proc,
                 sizeGbEachProcess = sizeGbEachProcess)
  IPs[grep(localHostEndIp, IPs)] <- "localhost"

  (table(IPs))
  (length(IPs))

  return(IPs)
}

#' @param machines `data.frame` of compute node information containing the following columns:
#'                 `ipEnd`, `availableCores`, `availableRam`
#' @export
makeIps <- function(machines,
                    ipStart,
                    proc,
                    nProcess,
                    sizeGbEachProcess) {
  proc <- tolower(proc)
  if (proc == "cores") {
    availableResource <- "availableCores"
    if (sum(machines$availableCores) < nProcess)
      warning("Not enough cores")
  } else {
    availableResource <- "availableRAM"
    if (sum(machines$availableRAM) < nProcess * sizeGbEachProcess)
      warning("Not enough RAM")
  }

  if (proc == "ram" || proc == "mem") {
    # find the number of cores for the amount of
    # RAM needed
    ncoresVector <- numeric()
    ncoresVector <- unlist(lapply(seq_along(machines$ipEnd), function(machineIndex){
      RAM <- machines$availableRAM[machineIndex]
      CORES <- machines$availableCores[machineIndex]
      newCore <- optimalClusterNumGeneralized(memRequiredMB = sizeGbEachProcess*1000,
                                              maxNumClusters = CORES,
                                              NumCoresAvailable = CORES,
                                              availMem = RAM*1000)
      ncoresVector <- c(ncoresVector, newCore)
    }))
    machines$coresByRAM <- ncoresVector
    availableResource <- "coresByRAM"
    nProcess <- min(sum(machines$coresByRAM), nProcess)
  }

  ipStart <- ifelse(grepl("[.]$", ipStart), ipStart, paste0(ipStart, "."))
  ipsEnd <- rep(machines$ipEnd,
                pmax(machines[[availableResource]],
                     ceiling(machines[[availableResource]] / (sum(machines[[availableResource]]) / nProcess))))

  ips <- paste0(ipStart, ipsEnd)
  i <- 0
  while (length(ips) > nProcess) {
    i <- i + 1
    i <- (i - 1) %% NROW(machines) + 1
    id <- which(endsWith(ips, suffix = as.character(machines$ipEnd[i])))[1]
    ips <- if (is.na(id)) {
      ips
    } else {
      ips[-id]
    }
  }
  return(sort(ips))
}

#' Determine the number of nodes to use in a new cluster
#'
#' Optimally determine the number of cores to use to set up a new cluster, based on:
#' \enumerate{
#'   \item the number of cores available (see note);
#'   \item the amount of free memory available on the local machine;
#'   \item the number of cores requested vs. the number available, such that if requesting
#'    more cores than available, the number of cores used will be adjusted to be a multiple
#'    of the number of cores needed, so jobs can be run in approximately-even-sized batches.
#'    (E.g., if 16 cores available but need 50, the time taken to run 3 batches of 16 plus
#'    a single batch of 2 -- i.e., 4 batches total -- is the same as running 4 batches of 13.)
#' }
#'
#' @param memRequiredMB The amount of memory needed in MB
#' @param maxNumClusters The number of nodes needed (requested)
#' @param NumCoresAvailable The number of cores available on the local machine (see note).
#' @param availMem The amount of free memory (RAM) available to use.
#'
#' @template nconnections
#'
#' @return integer specifying the number of cores
#'
#' @export
#' @importFrom parallelly availableConnections availableCores
#' @rdname optimalClusterNum
optimalClusterNumGeneralized <- function(memRequiredMB = 500,
                                         maxNumClusters = parallelly::availableCores(constraints = "connections"),
                                         NumCoresAvailable = parallelly::availableCores(constraints = "connections"),
                                         availMem = pemisc::availableMemory() / 1e+06) {
  NumCoresAvailable <- min(NumCoresAvailable, parallelly::availableConnections())

  if (maxNumClusters > 0) {
    if (is.null(availMem)) {
      message("Unable to estimate available memory. Returning 1 cluster.")
      numClusters <- 1L
    } else {
      nCoresAvail <- floor(min(NumCoresAvailable, availMem / memRequiredMB)) ## limit by avail RAM
      nBatches <- ceiling(maxNumClusters / nCoresAvail) ## if not enough cores, how many batches?
      nCores2Use <- ceiling(maxNumClusters / nBatches) ## reduce the 'ask' based on num of batches
      numClusters <- as.integer(nCores2Use)
    }
  } else {
    numClusters <- 1L
  }

  return(as.integer(numClusters))
}

#' @export
#' @rdname optimalClusterNum
optimalClusterNum <- function(memRequiredMB = 500, maxNumClusters = parallelly::availableCores(constraints = "connections")) {
  optimalClusterNumGeneralized(memRequiredMB = memRequiredMB, maxNumClusters = maxNumClusters)
}

#' Create a parallel fork cluster
#'
#' Given the size of a problem, it may not be useful to create a cluster.
#' This will make a fork cluster (so Linux only).
#'
#' @param useParallel Logical or numeric. If `FALSE`, returns NULL. If
#'        `numeric`, then will return a cluster object with this
#'        many cores, up to `maxNumClusters`
#'
#' @param MBper Numeric. Passed to `memRequiredMB` in
#'              [optimalClusterNum()]
#'
#' @param maxNumClusters Numeric or Integer. The theoretical upper limit
#'        for number of nodes to use with the cluster.
#'
#' @param assumeHyperThreads Logical. If `TRUE`, then it will more efficiently
#'   divide the `maxNumClusters` by `useParallel`, so that there is a
#'   lower number of cores used. This calculation may not be the ideal balance.
#'   A message will indicate the change from `maxNumClusters`, if there is one.
#'
#' @param ... Passed to `makeForkClusterRandom`.
#'            Only relevant for `iseed`.
#'
#' @export
#' @rdname makeOptimalCluster
makeOptimalCluster <- function(useParallel = getOption("pemisc.useParallel", FALSE),
                               MBper = 5e2, #nolint
                               maxNumClusters = parallelly::availableCores(constraints = "connections"),
                               assumeHyperThreads = FALSE, ...) {
  cl <- NULL
  if (is.null(maxNumClusters))
    maxNumClusters <- parallelly::availableCores(constraints = "connections")

  numClus <- if (isTRUE(useParallel)) {
    numClus <- optimalClusterNum(MBper, maxNumClusters = maxNumClusters)
    if (numClus <= 1) {
      numClus <- NULL
    }
    numClus
  } else if (is.numeric(useParallel)) {
    min(useParallel, maxNumClusters)
  }
  if (assumeHyperThreads && useParallel > maxNumClusters) {
    ceil <- ceiling(useParallel/maxNumClusters)
    numClusNew <- ceiling(useParallel/ceil)
    if (numClusNew != numClus) {
      message("assumeHyperThreads is TRUE; lowering from ", numClus, " to ",
              numClusNew, " for more efficient use of threads")
      numClus <- numClusNew
    }
  }

  dots <- list(...)
  if (!is.null(numClus)) {
    type <- if (is.null(list(...)$type)) {
      if (!identical("windows", .Platform$OS.type)) {
        "FORK"
      } else {
        "SOCK"
      }
    } else {
      dots$type
    }
    dots$type <- NULL
    cl <- do.call(makeClusterRandom, append(list(numClus, type = type), dots))
  }
  return(cl)
}

#' `makeForkCluster` with random seed set
#'
#' This will set different random seeds on the clusters (not the default)
#' with `makeForkCluster`.
#' It also defaults to creating a logfile with message of where it is.
#'
#' @param ... passed to `makeCluster`, e.g.,
#' @param iseed passed to `clusterSetRNGStream`
#'
#' @importFrom parallel clusterSetRNGStream makeForkCluster
#' @importFrom reproducible checkPath
#' @rdname makeClusterRandom
#' @importFrom parallel makeCluster clusterEvalQ clusterExport stopCluster
#' @param libraries A character vector of libraries to load in the SOCK cluster. This
#'   is ignored if a "FORK" cluster
#' @param objects a character string of objects that are required inside the SOCK cluster.
#'   Ignored if type != "SOCK"
#' @param envir Required if `objects` is passed. The environment where
#'   `objects` are found.
#' @inheritParams parallel::makeCluster
#' @export
makeClusterRandom <- function(..., type = "SOCK", iseed = NULL, libraries = NULL,
                              objects = NULL, envir = parent.frame()) {
  madeItToEnd <- FALSE
  dots <- list(...)
  if (!("outfile" %in% names(dots)) || is.null(dots$outfile)) {
    dots$outfile <- file.path("outputs", paste0("log_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".txt"))
  }
  checkPath(dirname(dots$outfile), create = TRUE)
  dots$type <- type
  for (i in 1:4)
    cat(file = dots$outfile, "------------------------------------------------------------")
  cl <- do.call(makeCluster, args = dots)
  on.exit({
    if (isFALSE(madeItToEnd)) {
      stopCluster(cl)
    }
  })
  message("  Starting a cluster with ", length(cl), " threads")
  message("    Log file is ", dots$outfile, ". To prevent log file, pass outfile = ''")
  clusterSetRNGStream(cl, iseed = iseed)

  env <- environment()
  if (identical(dots$type, "SOCK")) {
    if (!is.null(libraries)) {
      clusterExport(cl, varlist = list("libraries"), envir = env)
      clusterEvalQ(cl = cl, {
        lapply(libraries, require, character.only = TRUE)
      })
    }
    if (!is.null(objects)) {
      clusterExport(cl, varlist = objects, envir = envir)
    }

  }
  madeItToEnd <- TRUE
  cl
}

#' @export
#' @rdname makeClusterRandom
makeForkClusterRandom <- function(..., iseed = NULL) {
  makeClusterRandom(..., type = "FORK", iseed = iseed)
}

#' @rdname makeClusterRandom
#' @export
makeSockClusterRandom <- function(..., iseed = NULL) {
  makeClusterRandom(..., type = "SOCK", iseed = iseed)
}

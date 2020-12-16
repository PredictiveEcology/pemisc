#' Create IP addresses for network cluster
#'
#' \code{makeIpsForNetworkCluster} is a simple wrapper around \code{makeIps}.
#'
#' @param ipStart Network address prefix (i.e., the first, second, and third triplets of the IP address)
#' @param ipEnd Host IP address identifier (i.e., the final triplet of the IP address)
#' @param availableCores the number of available threads on each machine.
#' @param availableRAM the available RAM on each machine in GB
#' @param nProcess the number of processes
#' @param proc one of \code{"cores"} or \code{"ram"}, describing the limiting factor of the
#'             cluster computations
#' @param internalProcesses DESCRIPTION NEEDED
#' @param sizeGbEachProcess the size in GB of each process
#' @param localHostEndIp the address in \code{ipEnd} corresponding to local host
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

#' @param machines \code{data.frame} of compute node information containing the following columns:
#'                 \code{ipEnd}, \code{availableCores}, \code{availableRam}
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

#' @param memRequiredMB NEEDED
#' @param maxNumClusters DESCRIPTION NEEDED
#' @param NumCoresAvailable DESCRIPTION NEEDED
#' @param availMem DESCRIPTION NEEDED
#'
#' @return
#' DESCRIPTION NEEDED
#'
#' @export
#' @rdname makeIps
optimalClusterNumGeneralized <- function(memRequiredMB = 500,
                                         maxNumClusters = parallel::detectCores(),
                                         NumCoresAvailable = parallel::detectCores(),
                                         availMem = pemisc::availableMemory() / 1e+06) {
  if (maxNumClusters > 0) {
    if (!is.null(availMem)) {
      numClusters <- floor(min(NumCoresAvailable, availMem / memRequiredMB))
    }
    else {
      message("Unable to estimate available memory. Returning 1 cluster.")
      numClusters <- 1
    }
    numClusters <- min(maxNumClusters, numClusters, NumCoresAvailable)
  }
  else {
    numClusters <- 1
  }
  return(numClusters)
}

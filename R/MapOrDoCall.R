#' Determine the number of nodes to use in a new cluster
#'
#' TODO: DESCRIPTION NEEDED
#'
#' @param memRequiredMB The amount of memory needed in MB
#' @param maxNumClusters The maximum number of nodes to use
#'
#' @export
#' @rdname optimalClusterNum
optimalClusterNum <- function(memRequiredMB = 500, maxNumClusters = parallel::detectCores()) {
  if (Sys.info()["sysname"] == "Linux") {
    detectedNumCores <- parallel::detectCores()
    shouldUseCluster <- (maxNumClusters > 0)

    if (shouldUseCluster) {
      # try to scale to available RAM
      availMem <- availableMemory() / 1e6
      #try(aa <- system("free -lm", intern = TRUE))
      if (!is.null(availMem)) {
        numClusters <- floor(min(detectedNumCores, availMem / memRequiredMB))
      } else {
        message("The OS function, 'free' is not available. Returning 1 cluster")
        numClusters <- 1
      }
      numClusters <- min(maxNumClusters, numClusters, detectedNumCores)
    } else {
      numClusters <- 1
    }
  } else {
    message("This function returns 1 cluster on Windows and MacOS.")
    numClusters <- 1
  }
  return(numClusters)
}

#' Create a parallel Fork cluster, if useful
#'
#' Given the size of a problem, it may not be useful to create a cluster.
#' This will make a Fork cluster (so Linux only)
#' @param useParallel Logical or numeric. If \code{FALSE}, returns NULL. If
#'        \code{numeric}, then will return a cluster object with this
#'        many cores, up to \code{maxNumClusters}
#' @param MBper Numeric. Passed to \code{memRequiredMB} in
#'              \code{\link{optimalClusterNum}}
#' @param maxNumClusters Numeric or Integer. The theoretical upper limit
#'        for number of clusters to create (e.g., because there are only
#'        3 problems to solve, not \code{parallel::detectCores})
#' @param ... Passed to \code{makeForkClusterRandom}.
#'            Only relevant for \code{iseed}.
#'
#' @export
#' @rdname makeOptimalCluster
makeOptimalCluster <- function(useParallel = getOption("pemisc.useParallel", FALSE),
                               MBper = 5e2, #nolint
                               maxNumClusters = parallel::detectCores(), ...) {
  cl <- NULL
  if (is.null(maxNumClusters)) maxNumClusters <- parallel::detectCores()

    numClus <- if (isTRUE(useParallel)) {
      numClus <- optimalClusterNum(MBper, maxNumClusters = maxNumClusters)
      if (numClus <= 1) {
        numClus <- NULL
      }
      numClus
    } else if (is.numeric(useParallel)) {
      min(useParallel, maxNumClusters)
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

#' \code{makeForkCluster} with random seed set
#'
#' This will set different random seeds on the clusters (not the default)
#' with \code{makeForkCluster}.
#' It also defaults to creating a logfile with message of where it is.
#'
#' @param ... passed to \code{makeCluster}, e.g.,
#' @param iseed passed to \code{clusterSetRNGStream}
#'
#' @importFrom parallel clusterSetRNGStream makeForkCluster
#' @importFrom reproducible checkPath
#' @export
#' @rdname makeClusterRandom
makeForkClusterRandom <- function(..., iseed = NULL) {
  makeClusterRandom(..., type = "FORK", iseed = iseed)
}

#' @rdname makeForkClusterRandom
#' @export
makeSockClusterRandom <- function(..., iseed = NULL) {
  makeClusterRandom(..., type = "SOCK", iseed = iseed)
}

#' @rdname makeForkClusterRandom
#' @importFrom parallel makeCluster clusterEvalQ clusterExport stopCluster
#' @param libraries A character vector of libraries to load in the SOCK cluster. This
#'   is ignored if a "FORK" cluster
#' @param objects a character string of objects that are required inside the SOCK cluster.
#'   Ignored if type != "SOCK"
#' @param envir Required if \code{objects} is passed. The environment where
#'   \code{objects} are found.
#' @inheritParams parallel::makeCluster
#' @export
makeClusterRandom <- function(..., type = "SOCK", iseed = NULL, libraries = NULL,
                              objects = NULL, envir = parent.frame()) {
  madeItToEnd <- FALSE
  dots <- list(...)
  if (!("outfile" %in% names(dots))) {
    dots$outfile <- file.path("outputs", ".log.txt")
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


#' Find sources for arguments in arbitrary function(s)
#'
#' Search among local objects (which will often be arguments passed into a function) as well as
#' dot objects to match the formals needed by \code{fn}.
#' If \code{localFormalArgs} is named, then it will match the formal
#' (name of \code{localFormalArgs}) with the local object,
#' e.g., \code{localFormalArgs = c(x = "obj")} will find the object in the local environment called
#' \code{"obj"}, and this will be found because it matches the \code{x} argument in \code{fn}.
#'
#' @param fn Function name(?)
#' @param localFormalArgs A (named) character vector or arguments to
#' @param envir The environment in which to (???)
#' @param dots TODO: need description
#'
#' @return List of named objects. The names are the formals in \code{fn}, and
#' the objects are the values for those formals.
#' This can easily be passed to \code{do.call(fn, args1)}
#'
#' @export
#' @importFrom utils getFromNamespace
#' @rdname getLocalArgsFor
getLocalArgsFor <- function(fn, localFormalArgs, envir, dots) {
  fnicd <- getFromNamespace(".formalsNotInCurrentDots", "reproducible")
  if (missing(envir))
    envir <- parent.frame()
  if (missing(localFormalArgs))
    localFormalArgs <- ls(envir = envir)

  if (is.null(names(localFormalArgs)))
    names(localFormalArgs) <- localFormalArgs

  if (length(fn) > 1) {
    forms <- unlist(lapply(fn, fnicd, dots = dots))
    forms <- forms[duplicated(forms)]
  } else {
    forms <- fnicd(fn, dots = dots)
  }
  args <- dots[!(names(dots) %in% forms)]
  localFormals <- if (length(fn) > 1) {
    a <- lapply(fn, function(f)
      localFormalArgs[names(localFormalArgs) %in% formalArgs(f)]
    )
    unlist(a)[!duplicated(unlist(a))] # unique strips names
  } else {
    localFormalArgs[names(localFormalArgs) %in% formalArgs(fn)]
  }
  objsFromLocal <- mget(localFormals, envir = envir)
  names(objsFromLocal) <- names(localFormals)
  args <- append(args, objsFromLocal)
}

#' Identify the source for arguments passed to an arbitrary function
#'
#' When running arbitrary functions inside other functions, there is a common
#' construct in R to use \code{...}. It does not work, however, in the general case
#' to write \code{do.call(fn, list(...))} because not all \code{fn} themselves
#' accept \code{...}. So this will fail if too many arguments are supplied to
#' the \code{...}. In the general case, we want to write:
#' \code{do.call(fn, list(onlyTheArgumentsThatAreNeeded))}. This function helps
#' to find the \code{onlyTheArgumentsThatAreNeeded} by determining a) what is needed
#' by the \code{fn} (which can be a list of many \code{fn}), and b) where to find
#' values, either in an arbitrary environment or passed in via \code{dots}.
#'
#' @param fn A function or list of functions from which to run \code{formalArgs}
#' @param localFormalArgs A vector of possible objects, e.g., from \code{ls()}
#' @param envir The environment to find the objects named in \code{localFormalArgs}
#' @param dots Generally list(...), which would be an alternative place to find
#'             \code{localFormalArgs}
#'
#' @return A list of length 2, named \code{argsSingle} and \code{argsMulti}, which
#'         can be passed to e.g.,
#'         \code{MapOrDoCall(fn, multiple = args1$argsMulti, single = args1$argsSingle)}
#'
#' @export
#' @rdname identifyVectorArgs
identifyVectorArgs <- function(fn, localFormalArgs, envir, dots) {
  allArgs <- getLocalArgsFor(fn, localFormalArgs, envir = envir, dots = dots)

  # These types don't correctly work with "length", so omit them from search
  specialTypes <- c("environment", "SpatialPolygons", "RasterLayer", "RasterStack", "RasterBrick")
  lengthOne <- unlist(lapply(allArgs, is.null)) | unlist(lapply(allArgs, function(x) {
    if (any(unlist(lapply(specialTypes, is, obj = x))) | length(x) == 1) {
      TRUE
    } else {
      FALSE
    }}))
  if (sum(lengthOne)) {
    argsSingle <- allArgs[lengthOne]
    argsMulti <- allArgs[!lengthOne]
  } else {
    argsSingle <- list()
    argsMulti <- allArgs
  }

  list(argsSingle = argsSingle, argsMulti = argsMulti)
}

#' \code{Map}/\code{lapply} all in one
#'
#' Usually run after \code{identifyVectorArgs} which will separate the arguments
#' into vectors of values for a call to \code{Map}, and arguments that have
#' only one value (passed to \code{MoreArgs} in \code{Map}). If all are single
#' length arguments, then it will pass to \code{lapply}. If a \code{cl} is provided
#' and is non-\code{NULL}, then it will pass all arguments to \code{clusterMap} or
#' \code{clusterApply}.
#'
#' @param multiple This a list the arguments that Map will cycle over.
#' @param single Passed to \code{MoreArgs} in the \code{mapply} function.
#' @param fn The function that will be run via \code{Map}/\code{clusterMap}.
#' @param useCache Logical indicating whether to use the cache.
#' @param cl A cluster object or \code{NULL}.
#'
#' @export
#' @importFrom reproducible Cache
#' @rdname MapOrDoCall
#' @seealso \code{identifyVectorArgs}
MapOrDoCall <- function(fn, multiple, single, useCache, cl = NULL) { #nolint
  if (length(multiple)) {
    obj <- do.call(Cache, args = append(multiple, list(Map2, fn,
                                                       MoreArgs = single,
                                                       cl = cl,
                                                       useCache = useCache)))
  } else {
    if (!missing(useCache))
      single[["useCache"]] <- useCache
    obj <- do.call(Cache, args = append(list(fn), single))
  }
  obj
}

#' \code{Map} and \code{parallel::clusterMap} together
#'
#' This will send to \code{Map} or \code{clusterMap}, depending on whether \code{cl} is provided.
#' Because they use different argument names for the main function
#' to call, leave that argument unnamed.
#'
#' @param f passed as \code{f} to \code{Map} or \code{fun} to \code{clusterMap}
#' @param ... passed to \code{Map} or \code{clusterMap}
#' @param cl A cluster object, passed to \code{clusterMap}
#'
#' @export
#' @importFrom parallel clusterMap
#' @importFrom utils getFromNamespace
#' @rdname Map2
#' @examples
#'
#' \dontrun{
#' a <- 1:5
#' Map2(a, f = function(x) x)
#'
#' }
Map2 <- function(f, ..., cl = NULL) { #nolint
  #fnicd <- getFromNamespace(".formalsNotInCurrentDots", "reproducible")
  #formsMap <- fnicd(mapply, ...)
  #formsClusterMap <- fnicd(clusterMap, ...)
  argList <- list(...)
  if (any(c("fun", "FUN") %in% names(argList)))
    stop("Please use f, not fun or FUN, to supply the function.")
  #argList <- rlang::quos(...)
  if (is.null(cl)) {
    #browser()
    #argList <- list(...)
    # wrongFun1 <- "fun" %in% names(argList)
    # if (wrongFun1) {
    #   fun <- argList$fun
    # }
    # wrongFun2 <- "FUN" %in% names(argList)
    # if (wrongFun2) {
    #   fun <- argList$FUN
    #   argList$FUN <- NULL
    # }
    # argList[setdiff(formsMap, formsClusterMap)] <- NULL
    # if (wrongFun1 || wrongFun2) {
    #   argList$f <- fun
    #   argList$fun <- NULL
    # }
    # eval_tidy(eval_tidy(quos(Map(!!!argList)))[[1]])
    Map(f = f, ...)
    #Map(!!!(argList))
    #do.call(Map, args = argList)
  } else {
    #argList <- list(...)
    # wrongFun1 <- "f" %in% names(argList)
    # if (wrongFun1) {
    #   fun <- argList$f
    #   argList$fun <- NULL
    # }
    # wrongFun2 <- "FUN" %in% names(argList)
    # if (wrongFun2) {
    #   fun <- argList$FUN
    #   argList$FUN <- NULL
    # }
    # argList[setdiff(formsClusterMap, formsMap)] <- NULL
    # if (wrongFun1 || wrongFun2) {
    #   argList$fun <- fun
    # }
    # argList$cl <- quo(cl)
    clusterMap(cl = cl, fun = f, ...)
    #eval_tidy(eval_tidy(quos(clusterMap(!!!argList)))[[1]])
    #do.call(clusterMap, append(list(cl = cl), argList))
  }
}

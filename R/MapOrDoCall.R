#' Find sources for arguments in arbitrary function(s)
#'
#' Search among local objects (which will often be arguments passed into a function) as well as
#' dot objects to match the formals needed by `fn`.
#' If `localFormalArgs` is named, then it will match the formal
#' (name of `localFormalArgs`) with the local object,
#' e.g., `localFormalArgs = c(x = "obj")` will find the object in the local environment called
#' `"obj"`, and this will be found because it matches the `x` argument in `fn`.
#'
#' @param fn Function name(?)
#' @param localFormalArgs A (named) character vector or arguments to
#' @param envir The environment in which to (???)
#' @param dots TODO: need description
#'
#' @return List of named objects. The names are the formals in `fn`, and
#' the objects are the values for those formals.
#' This can easily be passed to `do.call(fn, args1)`
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
#' construct in R to use `...`. It does not work, however, in the general case
#' to write `do.call(fn, list(...))` because not all `fn` themselves
#' accept `...`. So this will fail if too many arguments are supplied to
#' the `...`. In the general case, we want to write:
#' `do.call(fn, list(onlyTheArgumentsThatAreNeeded))`. This function helps
#' to find the `onlyTheArgumentsThatAreNeeded` by determining a) what is needed
#' by the `fn` (which can be a list of many `fn`), and b) where to find
#' values, either in an arbitrary environment or passed in via `dots`.
#'
#' @param fn A function or list of functions from which to run `formalArgs`
#' @param localFormalArgs A vector of possible objects, e.g., from `ls()`
#' @param envir The environment to find the objects named in `localFormalArgs`
#' @param dots Generally list(...), which would be an alternative place to find
#'             `localFormalArgs`
#'
#' @return A list of length 2, named `argsSingle` and `argsMulti`, which
#'         can be passed to e.g.,
#'         `MapOrDoCall(fn, multiple = args1$argsMulti, single = args1$argsSingle)`
#'
#' @export
#' @rdname identifyVectorArgs
identifyVectorArgs <- function(fn, localFormalArgs, envir, dots) {
  allArgs <- getLocalArgsFor(fn, localFormalArgs, envir = envir, dots = dots)

  # These types don't correctly work with "length", so omit them from search
  specialTypes <- c("environment", "sf", "Spatial", "Raster")
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

#' `Map`/`lapply` all in one
#'
#' Usually run after `identifyVectorArgs` which will separate the arguments
#' into vectors of values for a call to `Map`, and arguments that have
#' only one value (passed to `MoreArgs` in `Map`). If all are single
#' length arguments, then it will pass to `lapply`. If a `cl` is provided
#' and is non-`NULL`, then it will pass all arguments to `clusterMap` or
#' `clusterApply`.
#'
#' @param multiple This a list the arguments that Map will cycle over.
#' @param single Passed to `MoreArgs` in the `mapply` function.
#' @param fn The function that will be run via `Map`/`clusterMap`.
#' @param useCache Logical indicating whether to use the cache.
#' @param cl A cluster object or `NULL`.
#'
#' @export
#' @importFrom reproducible Cache
#' @rdname MapOrDoCall
#' @seealso `identifyVectorArgs`
MapOrDoCall <- function(fn, multiple, single, useCache = FALSE, cl = NULL) { #nolint
  if (length(multiple)) {
    browser(expr = exists("._MapOrDoCall_1"))

    ## TODO: Cache not handling passthrough (i.e., useCache = FALSE) in some cases
    ## 2022-10-04: changed to make conditional on `useCache`
    # obj <- do.call(Cache, args = append(multiple, alist(Map2, fn,
    #                                                     MoreArgs = single,
    #                                                     cl = cl,
    #                                                     useCache = useCache)))
    if (isTRUE(useCache)) {
      obj <- Cache(do.call, args = append(multiple, alist(Map2, fn, MoreArgs = single, cl = cl)))
    } else {
      obj <- do.call(Map2, args = append(multiple, alist(fn, MoreArgs = single, cl = cl)))
    }
  } else {
    browser(expr = exists("._MapOrDoCall_2"))

    ## TODO: Cache not handling passthrough (i.e., useCache = FALSE) in some cases
    ## 2022-10-04: changed to make conditional on `useCache`
    # single[["useCache"]] <- useCache
    # obj <- do.call(Cache, args = append(alist(fn), single))
    if (isTRUE(useCache)) {
      single[["useCache"]] <- useCache
      obj <- do.call(Cache, args = append(alist(fn), single))
    } else {
      obj <- do.call(Cache, args = append(alist(fn), single))
    }
  }
  obj
}

#' `Map` and `parallel::clusterMap` together
#'
#' This will send to `Map` or `clusterMap`, depending on whether `cl` is provided.
#' Because they use different argument names for the main function
#' to call, leave that argument unnamed.
#'
#' @param f passed as `f` to `Map` or `fun` to `clusterMap`
#' @param ... passed to `Map` or `clusterMap`
#' @param cl A cluster object, passed to `clusterMap`
#'
#' @export
#' @importFrom parallel clusterMap
#' @rdname Map2
#' @examples
#'
#' \dontrun{
#' a <- 1:5
#' Map2(a, f = function(x) x)
#' }
Map2 <- function(f, ..., cl = NULL) {
  argList <- list(...)
  if (any(c("fun", "FUN") %in% names(argList)))
    stop("Please use f, not fun or FUN, to supply the function.")

  if (is.null(cl)) {
    Map(f = f, ...)
  } else {
    clusterMap(cl = cl, fun = f, ...)
  }
}

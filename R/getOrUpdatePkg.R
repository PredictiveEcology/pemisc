getOrUpdatePkg <- function(p, minVer = "0") {
  repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
  if (length(p) != length(minVer)) stop("minVer must be as long as p")
  p <- Map(pp = p, mv = minVer, function(pp, mv) {
    if (!isFALSE(try(packageVersion(p) < mv, silent = TRUE) )) {
      p
    } else {
      NULL
    }
  }
  )
  if (length(unlist(p)))
    install.packages(unlist(p), repos = repo)
}

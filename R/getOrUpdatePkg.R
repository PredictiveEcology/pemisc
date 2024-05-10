getOrUpdatePkg <- function(p, minVer = "0") {
  repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
  p <- lapply(p, function(pp) {
    if (!isFALSE(try(packageVersion(pp) < minVer, silent = TRUE) )) {
      pp
    } else {
      NULL
    }
  }
  )
  if (length(unlist(p)))
    install.packages(unlist(p), repos = repo)
}

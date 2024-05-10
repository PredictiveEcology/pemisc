getOrUpdatePkg <- function(p, minVer = "0") {
  p <- lapply(p, function(pp) {
    if (!isFALSE(try(packageVersion(pp) < minVer, silent = TRUE) )) {
      repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
      pp
    } else {
      NULL
    }
  }
  )
  if (length(unlist(p)))
    install.packages(unlist(p), repos = repo)
}

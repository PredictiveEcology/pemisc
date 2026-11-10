## A local `.prj` served through a file:// URL, so no test touches the network.
local_prj_source <- function(env = parent.frame()) {
  src <- file.path(withr::local_tempdir(.local_envir = env), "source.prj")
  writeLines("PROJCS[\"test\"]", src)
  paste0("file://", normalizePath(src))
}

test_that("createPrjFile() writes the .prj next to a single shapefile", {
  url <- local_prj_source()
  d <- withr::local_tempdir()
  shp <- file.path(d, "a.shp")
  file.create(shp)
  withr::local_dir(withr::local_tempdir()) ## a .prj must not appear in the working directory

  prj <- createPrjFile(shp, urlForProj = url)

  expect_identical(prj, file.path(d, "a.prj"))
  expect_identical(readLines(prj), "PROJCS[\"test\"]")
  expect_false(file.exists(".prj"))
})

test_that("createPrjFile() keeps an existing .prj and gives one per shapefile", {
  url <- local_prj_source()
  d <- withr::local_tempdir()
  writeLines("PROJCS[\"kept\"]", file.path(d, "a.prj"))

  prj <- createPrjFile(file.path(d, c("a.shp", "a.dbf", "b.shp")), urlForProj = url)

  expect_identical(prj, file.path(d, c("a.prj", "b.prj")))
  expect_identical(readLines(file.path(d, "a.prj")), "PROJCS[\"kept\"]")
  expect_identical(readLines(file.path(d, "b.prj")), "PROJCS[\"test\"]")
})

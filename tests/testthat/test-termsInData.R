test_that("termsInData() is deprecated and otherwise unchanged", {
  dat <- data.frame(y = 1, x = 1, z = 1)
  expect_warning(
    out <- termsInData(quote(stats::lm(y ~ x + z)), dat),
    "deprecated"
  )
  expect_identical(out, c("x", "z"))

  ## the reason for deprecation: a transformed response comes back as a term
  expect_warning(
    out <- termsInData(quote(stats::lm(log(y) ~ x)), dat),
    "deprecated"
  )
  expect_identical(out, c("y", "x"))
})

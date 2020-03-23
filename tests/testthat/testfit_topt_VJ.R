library(testthat)
library(plantecowrap)
context("Fitting temperature responses")

df <- data.frame(Vcmax = c(5.5, 20.5, 62.3, 90, 113.6, 142.3),
                 Tleaf = c(10, 15, 20, 25, 30, 35),
                 Jmax = c(16.6, 38.2, 79.8, 148.7, 273.7, 407.6))

tresp <- suppressWarnings(fit_topt_VJ(df))


test_that("Outputs", {
  expect_is(object = tresp, class = "list")
  expect_is(object = tresp[[1]], class = "data.frame")
  expect_is(object = tresp[[2]], class = "data.frame")
  expect_length(object = tresp, 3)
})

unlink("df", recursive = FALSE)
unlink("tresp", recursive = FALSE)

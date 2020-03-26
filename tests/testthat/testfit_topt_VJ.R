library(testthat)
library(plantecowrap)
context("Fitting temperature responses")
data <- read.csv(system.file("extdata", "example_1.csv",
                             package = "plantecowrap"),
                 stringsAsFactors = FALSE)
fits <- fitacis2(data = data,
                 varnames = list(ALEAF = "A",
                                 Tleaf = "Tleaf",
                                 Ci = "Ci",
                                 PPFD = "PPFD",
                                 Rd = "Rd",
                                 Press = "Press"),
                 group1 = "Treat",
                 fitTPU = FALSE,
                 fitmethod = "bilinear",
                 gm25 = 10000,
                 Egm = 0)
outputs <- acisummary(data, group1 = "Treat", fits = fits)
tresp <- suppressWarnings(fit_topt_VJ(outputs))
test_that("Outputs", {
  expect_is(object = tresp, class = "list")
  expect_is(object = tresp[[1]], class = "data.frame")
  expect_is(object = tresp[[2]], class = "data.frame")
  expect_length(object = tresp, 3)
})

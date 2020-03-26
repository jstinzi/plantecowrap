library(testthat)
library(plantecowrap)
context("Fitting ACi Curves")
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
test_that("Outputs", {
  expect_is(object = fits, class = "list")
  expect_is(object = outputs, class = "data.frame")
  expect_length(object = outputs, 12)
  expect_length(object = outputs$Vcmax, 6)
})

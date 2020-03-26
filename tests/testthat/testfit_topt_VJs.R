library(testthat)
library(plantecowrap)
context("Fitting multiple temperature responses")
data <- read.csv(system.file("extdata", "example_2.csv",
                              package = "plantecowrap"),
                  stringsAsFactors = FALSE)
fits2 <- fitacis2(data = data,
                  varnames = list(ALEAF = "A",
                                  Tleaf = "Tleaf",
                                  Ci = "Ci",
                                  PPFD = "PPFD",
                                  Rd = "Rd",
                                  Press = "Press"),
                  group1 = "Grouping",
                  fitTPU = FALSE,
                  fitmethod = "bilinear",
                  gm25 = 10000,
                  Egm = 0)
outputs <- acisummary(data, group1 = "Grouping", fits = fits2)
outputs <- separate(outputs, col = "ID", into = c("Treat", "Block"), sep = "_")
tresps <- suppressWarnings(fit_topt_VJs(outputs, group = "Block"))
pars <- get_t_pars(tresps)
graphs <- get_t_graphs(tresps)
test_that("Outputs", {
  expect_is(object = tresps, class = "list")
  expect_is(object = tresps[[1]], class = "list")
  expect_is(object = tresps[[2]], class = "list")
  expect_is(object = pars, class = "data.frame")
  expect_is(object = graphs, class = "list")
  expect_length(object = tresps, 2)
  expect_length(object = tresps[[1]], 3)
  expect_length(object = tresps[[2]], 3)
  expect_length(object = pars, 8)
  expect_length(object = pars$Ea, 4)
  expect_length(object = graphs, 2)
})

library(testthat)
library(plantecowrap)
context("Fitting multiple temperature responses")

df <- data.frame(Vcmax = c(5.5, 20.5, 62.3, 90, 113.6, 142.3),
                 Tleaf = c(10, 15, 20, 25, 30, 35),
                 Jmax = c(16.6, 38.2, 79.8, 148.7, 273.7, 407.6))

df2 <- data.frame(Vcmax = 2 * c(5.5, 20.5, 62.3, 90, 113.6, 142.3),
                 Tleaf = c(10, 15, 20, 25, 30, 35),
                 Jmax = 2 * c(16.6, 38.2, 79.8, 148.7, 273.7, 407.6))

df2 <- rbind(df, df2)

df2$Block <- c(rep("a", nrow(df)),
                 rep("b", nrow(df)))

tresps <- suppressWarnings(fit_topt_VJs(df2, group = "Block"))

#Get parameters
pars <- get_t_pars(tresps)

#Get graphs
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

unlink("df", recursive = FALSE)
unlink("df2", recursive = FALSE)
unlink("tresps", recursive = FALSE)
unlink("pars", recursive = FALSE)
unlink("graphs", recursive = FALSE)
library(testthat)
library(plantecowrap)
context("Fitting ACi Curves")

Tleaf <- seq(from = 10, to = 35, by = 5)
Tleaf <- sort(rep(Tleaf, 15))

Vcmax_start <- 90 * modarrhenius(Ea = 35, Hd = 220, dS = 0.3, Tleaf = Tleaf)
Jmax_start <- 140 * modarrhenius(Ea = 80, Hd = 220, dS = 0.6, Tleaf = Tleaf)
R_start <- 1.5 * arrhenius(Ea = 20, Tleaf = Tleaf)
GammaStar_start <- 42.75 * arrhenius(Ea = 37.83, Tleaf = Tleaf)
Km_start <- 718.40 * arrhenius(Ea = 65.50828, Tleaf = Tleaf)
Ci <- rep(c(75, 100, 150, 200, 250,
            300, 350, 400, 600, 800,
            1000, 1200, 1400, 1600, 1800), 6)
A <- 1:90
Vc <- 1:90
Vj <- 1:90
for (i in seq_along(A)) {
  Vc[i] <- (Vcmax_start[i] * (Ci[i] - GammaStar_start[i]) /
              (Ci[i] + Km_start[i]))
  Vj[i] <- (Jmax_start[i] * (Ci[i] - GammaStar_start[i]) /
              (4 * Ci[i] + 8 * GammaStar_start[i]))
  if (Vc[i] < 0) {
    Vc[i] <- NA
  }
  if (Vj[i] < 0) {
    Vj[i] <- NA
  }
  A[i] <- min(Vc[i], Vj[i]) - R_start[i]
}

PPFD <- rep(2000, 90)
Press <- rep(100, 90)

data <- data.frame(cbind(A,
                         Ci,
                         Tleaf,
                         Press,
                         PPFD))

data$Treat <- as.character(Tleaf)

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

unlink("Tleaf", recursive = FALSE)
unlink("Vcmax_start", recursive = FALSE)
unlink("Jmax_start", recursive = FALSE)

unlink("R_start", recursive = FALSE)
unlink("GammaStar_start", recursive = FALSE)
unlink("Km_start", recursive = FALSE)

unlink("A", recursive = FALSE)
unlink("Vc", recursive = FALSE)
unlink("Vj", recursive = FALSE)

unlink("PPFD", recursive = FALSE)
unlink("Press", recursive = FALSE)
unlink("data", recursive = FALSE)

unlink("fits", recursive = FALSE)
unlink("outputs", recursive = FALSE)

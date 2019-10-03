#' Fitting the Topt temperature response model
#'
#' @param param25 parameter value at 25 C
#' @param Ea activation energy in kJ/mol
#' @param Hd deactivation energy in kJ/mol
#' @param kopt optimum parameter value
#' @param Tleaf leaf temperature in K
#' @param Topt optimum leaf temperature in K
#'
#' @return toptfit is used to fit a Topt model
#' @export
#'
toptfit <- function(Ea, Hd, kopt, Tleaf, Topt){
  param = kopt * (Hd * exp( (Ea * (Tleaf - Topt) /
                               (Tleaf * Topt * 0.008314)))) /
    (Hd - Ea * (1 - exp( (Ea * (Tleaf - Topt) /
                            (Tleaf * Topt * 0.008314)))))
}
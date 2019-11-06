#' Fitting the Topt temperature response model
#'
#' @param Ea activation energy in kJ/mol
#' @param Hd deactivation energy in kJ/mol
#' @param kopt optimum parameter value
#' @param Tleaf leaf temperature in Celsius
#' @param Topt optimum leaf temperature in Celsius
#'
#' @return toptfit is used to fit a Topt model
#' @export
#'
toptfit <- function(Ea, Hd, kopt, Tleaf, Topt){
  param = kopt * (Hd * exp( (Ea * (Tleaf - Topt) /
                               ((Tleaf + 273.15) * (Topt + 273.15) * 0.008314)))) /
    (Hd - Ea * (1 - exp( (Hd * (Tleaf - Topt) /
                            ((Tleaf + 273.15) * (Topt + 273.15) * 0.008314)))))
}

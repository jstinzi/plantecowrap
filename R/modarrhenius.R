#' Fitting the peaked Arrhenius temperature response model
#'
#' @param Ea activation energy in kJ/mol
#' @param Tleaf leaf temperature in Celsius
#' @param dS entropy parameter in kJ/mol
#' @param Hd deactivation energy in kJ/mol
#'
#' @return modarrhenius is used to fit a peaked model
#' @export
#'
modarrhenius <- function(Ea, Hd, dS, Tleaf){
  param = exp(Ea * ((Tleaf + 273.15) - 298.15) / 
                          (298.15 * (Tleaf + 273.15) * 0.008314)) *
    ( (1 + exp( (298.15 * dS - Hd) / (298.15 * 0.008314))) /
        (1 + exp( ((Tleaf + 273.15) * dS - Hd) / ((Tleaf + 273.15) * 0.008314))))
}

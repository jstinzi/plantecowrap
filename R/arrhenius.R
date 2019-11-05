#' Fitting the Arrhenius temperature response model
#'
#' @param Ea activation energy in kJ/mol
#' @param Tleaf leaf temperature in Celsius
#'
#' @return arrhenius is used to fit an exponential model
#' @export
#'
arrhenius <- function(Ea, Tleaf){
  param = exp(Ea * ((Tleaf + 273.15) - 298.15) / 
                          (298.15 * (Tleaf + 273.15) * 0.008314))
}

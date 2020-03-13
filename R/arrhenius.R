#' The Arrhenius temperature response equation
#'
#' @param Ea activation energy in kJ/mol
#' @param Tleaf leaf temperature in Celsius
#'
#' @return arrhenius is an exponential model
#' 
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#' @export
#'
arrhenius <- function(Ea, Tleaf){
  param = exp(Ea * ((Tleaf + 273.15) - 298.15) / 
                          (298.15 * (Tleaf + 273.15) * 0.008314))
}

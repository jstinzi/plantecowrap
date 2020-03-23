#' The Arrhenius temperature response equation
#'
#' @param Ea activation energy in kJ mol-1
#' @param Tleaf leaf temperature in Celsius
#'
#' @return arrhenius is an exponential temperature response model. This
#' function automatically converts temperature from Celsius to Kelvin for
#' the calculation.
#' 
#' REFERENCE
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#' @export
#'
arrhenius <- function(Ea, Tleaf){
  param = exp(Ea * ((Tleaf + 273.15) - 298.15) / 
                          (298.15 * (Tleaf + 273.15) * 0.008314))
}

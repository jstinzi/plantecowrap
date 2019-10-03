#' Fitting the Arrhenius temperature response model
#'
#' @param param25 parameter value at 25 C
#' @param Ea activation energy in kJ/mol
#' @param Tleaf leaf temperature in K
#'
#' @return arrhenius is used to fit an exponential model
#' @export
#'
arrhenius <- function(param25, Ea, Tleaf){
  param = param25 * exp(Ea * (Tleaf - 298.15) / 
                          (298.15 * Tleaf * 0.008314))
}
#' Fitting the peaked Arrhenius temperature response model
#'
#' @param param25 parameter value at 25 C
#' @param Ea activation energy in kJ/mol
#' @param Tleaf leaf temperature in K
#' @param dS entropy parameter in kJ/mol
#' @param Hd deactivation energy in kJ/mol
#'
#' @return modarrhenius is used to fit a peaked model
#' @export
#'
modarrhenius <- function(param25, Ea, Hd, dS, Tleaf){
  param = param25 * exp(Ea * (Tleaf - 298.15) / 
                          (298.15 * Tleaf * 0.008314)) *
    ( (1 + exp( (298.15 * dS - Hd) / (298.15 * 0.008314))) /
        (1 + exp( (Tleaf * dS - Hd) / (Tleaf * 0.008314))))
}
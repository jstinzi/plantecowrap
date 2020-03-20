#' Fitting the peaked Arrhenius temperature response model
#'
#' @param Ea activation energy in kJ mol-1
#' @param Tleaf leaf temperature in Celsius
#' @param dS entropy of deactivation in kJ mol-1
#' @param Hd deactivation energy in kJ mol-1
#'
#' @return modarrhenius is used to fit a peaked Arrhenius model.
#' 
#' REFERENCE
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' @export
#'
modarrhenius <- function(Ea, Hd, dS, Tleaf){
  param = exp(Ea * ((Tleaf + 273.15) - 298.15) / 
                          (298.15 * (Tleaf + 273.15) * 0.008314)) *
    ( (1 + exp( (298.15 * dS - Hd) / (298.15 * 0.008314))) /
        (1 + exp( ((Tleaf + 273.15) * dS - Hd) / 
                    ((Tleaf + 273.15) * 0.008314))))
}

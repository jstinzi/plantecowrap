#' Fitting the Topt temperature response model
#'
#' @param Ea activation energy in kJ mol-1
#' @param Hd deactivation energy in kJ mol-1
#' @param kopt parameter value at optimum temperature
#' @param Tleaf leaf temperature in Celsius
#' @param Topt optimum leaf temperature in Celsius
#'
#' @return toptfit is the Topt temperature response model.
#' 
#' REFERENCE
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' @export
#'
toptfit <- function(Ea, Hd, kopt, Tleaf, Topt){
  param = kopt * (Hd * exp( (Ea * (Tleaf - Topt) /
                               ((Tleaf + 273.15) * 
                                  (Topt + 273.15) * 0.008314)))) /
    (Hd - Ea * (1 - exp( (Hd * (Tleaf - Topt) /
                            ((Tleaf + 273.15) * 
                               (Topt + 273.15) * 0.008314)))))
}

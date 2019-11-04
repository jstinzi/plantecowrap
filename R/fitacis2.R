#' Fit A-Ci curves with custom kinetics
#'
#' @param data data frame with A/Ci curves
#' @param group1 grouping variable 1, could be species, temperature, ID
#' @param group2 grouping variable 2
#' @param group3 grouping variable 3
#' @param gm25 mesophyll conductance at 25C
#' @param Egm activation energy of mesophyll conductance in kJ/mol
#' @param K25 Km at 25C
#' @param Ek activation energy of Km in kJ/mol
#' @param fitmethod See ?fitaci in plantecophys
#' @param fitTPU See ?fitaci in plantecophys. Set to TRUE/FALSE
#' @param Tcorrect See ?fitaci in plantecophys. Default here is FALSE
#' @param useRd See ?fitaci in plantecophys
#' @param citransition See ?fitaci in plantecophys
#' @param alphag See ?fitaci in plantecophys
#' @param PPFD See ?fitaci in plantecophys
#' @param Tleaf See ?fitaci in plantecophys
#' @param alpha See ?fitaci in plantecophys
#' @param theta See ?fitaci in plantecophys
#' @param varnames See ?fitaci in plantecophys
#'
#' @return fitacis2 allows gmeso, GammaStar, and Km to vary with Tleaf
#'         output matches the fitacis function from plantecophys
#' @importFrom tidyr unite
#' @importFrom plantecophys fitaci
#' @export
#'
fitacis2 <- function(data,
                     group1,
                     group2,
                     group3,
                     gm25,
                     Egm,
                     K25,
                     Ek,
                     fitmethod,
                     fitTPU,
                     Tcorrect,
                     useRd = FALSE,
                     citransition = NULL,
                     alphag = 0,
                     PPFD = NULL,
                     Tleaf = NULL,
                     alpha = 0.24,
                     theta = 0.85,
                     varnames = list(ALEAF = "Photo", 
                                     Tleaf = "Tleaf", 
                                     Ci = "Ci",
                                     PPFD = "PARi", 
                                     Rd = "Rd")){
  data$group1 <- data[,group1]
  data$group2 <- data[,group2]
  data$group3 <- data[,group3]
  
  data <- unite(data, col = "group",
                c("group1", "group2", "group3"),
                sep = "_")
  
  data <- split(data, data$group)
  fits <- as.list(1:length(data))
  
  for (i in 1:length(data)){
    gmeso <- gm25 * exp(Egm * 
                          (mean(data[[i]]$Tleaf + 
                                  273.15) - 298.15) /
                          (298.15 * 
                             mean(data[[i]]$Tleaf + 273.15) *
                             0.008314))
    
    Km <- K25 * exp(Ek * 
                      (mean(data[[i]]$Tleaf + 
                              273.15) - 298.15) /
                      (298.15 * 
                         mean(data[[i]]$Tleaf + 273.15) *
                         0.008314))
    Patm <- mean(data[[i]]$Press)
    
    fits[[i]] <- fitaci(data[[i]], 
                        Patm = Patm,
                        varnames = varnames,
                        fitmethod = fitmethod, 
                        Tcorrect = Tcorrect,
                        fitTPU = fitTPU,
                        gmeso = gmeso, 
                        Km = Km,
                        useRd = useRd,
                        citransition = citransition,
                        alphag = alphag,
                        PPFD = PPFD,
                        Tleaf = Tleaf,
                        alpha = alpha,
                        theta = theta,)
    names(fits)[i] <- data[[i]]$group[1]
  }
  return(fits)
}
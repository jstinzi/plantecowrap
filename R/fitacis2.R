#' Fit A-Ci curves with custom kinetics
#'
#' @param data data frame with A/Ci curves. Requires net CO2 assimilation 
#' (Anet/Photo/ALEAF in umol m-2 s-1), leaf temperature (Tleaf in Celsius),
#' intercellular CO2 concentration (Ci, in umol mol-1), incident irradiance
#' on the leaf (PPFD/PARi in umol m-2 s-1), atmospheric pressure (Patm/Press
#' in kPa), and (optional, set useRd = TRUE for this option) respiration 
#' (Rd, in umol m-2 s-1).
#' @param group1 grouping variable 1, could be species, temperature, ID
#' @param group2 grouping variable 2
#' @param group3 grouping variable 3
#' @param gm25 mesophyll conductance at 25 Celsius in mol m-2 s-1 bar-1
#' @param Egm activation energy of mesophyll conductance in kJ mol-1
#' @param K25 Km in 21% O2 (aka Kcair, aka apparent Km in 21% O2) at 25 Celsius
#' in umol mol-1 (equivalent to ubar bar-1)
#' @param Ek activation energy of Kcair in kJ mol-1
#' @param Gstar25 photorespiratory CO2 compensation point at 25 Celsius in 
#' umol mol-1 (equivalent to ubar bar-1)
#' @param Egamma activation energy of GammaStar in kJ mol-1
#' @param fitmethod Set to either "bilinear" or "default". Default option
#' in this package is "default". See ?fitaci in plantecophys for more
#' details.
#' @param fitTPU Should TPU limitations be fit? Set to TRUE/FALSE. See 
#' ?fitaci in plantecophys for more details.
#' @param Tcorrect Should outputs be temperature corrected? Default here 
#' is FALSE. See ?fitaci in plantecophys for more details.
#' @param useRd Should respiration be used? Default is FALSE. See ?fitaci 
#' in plantecophys for more details.
#' @param citransition Pre-specify Ci transition point? Units in umol mol-1 
#' (ubar bar-1) Default is FALSE. See ?fitaci in plantecophys for more details.
#' @param alphag Fraction of photorespiratory glycolate carbon that is not 
#' returned to the chloroplast (von Caemmerer, 2000). If ACi curves show 
#' high-CO2 decline, then this value should be > 0. See ?fitaci in plantecophys 
#' for more details.
#' @param PPFD Light intensity? Can be retrieved from dataframe. Default is
#' NULL. Units are umol m-2 s-1. See ?fitaci in plantecophys for more details.
#' @param Tleaf Leaf temperature? Can be retrieved from dataframe. Default is
#' NULL. Units are Celsius. See ?fitaci in plantecophys for more details.
#' @param alpha Quantum yield of CO2 assimilation. Default is 0.24. Units are
#' umol CO2 fixed / umol photons. See  ?fitaci in plantecophys for more details.
#' @param theta Curvature of the photosynthetic light response. Default is
#' 0.85. If light response has sharper transition, increase up to 1. If light
#' response has shallower curves, decrease towards 0. See ?fitaci in 
#' plantecophys for more details.
#' @param varnames Variable names in your dataframe. ALEAF is net CO2
#' assimilation in umol m-2 s-1, Tleaf is leaf temperature in Celsius, 
#' Ci is intercellular CO2 concentration in umol mol-1, PPFD is light
#' intensity in umol m-2 s-1, Rd is respiration rate in umol m-2 s-1, 
#' and Press is atmospheric pressure in kPa. See ?fitaci in plantecophys 
#' for more details.
#'
#' @return fitacis2 allows gmeso, GammaStar, and Km to vary with Tleaf.
#' Output matches the fitacis function from plantecophys. Notethat the 
#' temperature response function of Km is derived from the temperature 
#' responses of Ko and Kc in Bernacchi et al.2001, as is the GammaStar 
#' temperature response defaults. The gm defaults are from Bernacchi et 
#' al. 2002 fitted between 1and 35 Celsius. Also note that this ALWAYS 
#' uses gm. To fitdata on a "Ci-basis", set gm25 really high (e.g. 
#' 10000 mol m-2 s-1 bar-1) and Egm to 0 kJ mol-1. 
#'         
#' REFERENCES
#' Bernacchi CJ, Singsaas EL, Pimentel C, Portis AR, Long SP. 
#' 2001. Improved temperature response functions for models of 
#' rubisco-limited photosynthesis. Plant Cell Environment 24:253-259.
#'         
#' Bernacchi CJ, Portis AR, Nakano H, von Caemmerer S, Long SP. 2002.
#' Temperature response of mesophyll conductance. Implications for the
#' determination of rubisco enzyme kinetics and for limitations to 
#' photosynthesis in vivo. Plant Physiology 130:1992-1998.
#'         
#' von Caemmerer S. 2000. Biochemical models of leaf photosynthesis. 
#' CSIRO Publishing, Collingwood.
#' @importFrom tidyr unite
#' @importFrom plantecophys fitaci
#' @export
#' 
#' @examples \dontrun{
#' data <- read.csv("mydata.csv")
#' fits <- fitacis2(data, group1 = "a", group2 = "b", group3 = "c",
#' fitmethod = "bilinear", fitTPU = TRUE, Tcorrect = FALSE)
#' }
#'
fitacis2 <- function(data,
                     group1,
                     group2 = NA,
                     group3 = NA,
                     gm25 = 0.08701, #mol m-2 s-1 bar-1
                     Egm = 47.650, #kJ mol-1
                     K25 = 718.40, #umol mol-1 (ubar bar-1)
                     Ek = 65.50828, #kJ mol-1
                     Gstar25 = 42.75, #umol mol-1 (ubar bar-1)
                     Egamma = 37.83, #kJ mol-1
                     fitmethod = "default",
                     fitTPU = TRUE,
                     Tcorrect = FALSE,
                     useRd = FALSE,
                     citransition = NULL, #umol mol-1
                     alphag = 0,
                     PPFD = NULL, #umol m-2 s-1
                     Tleaf = NULL, #Celsius
                     alpha = 0.24, #umol CO2 / umol photons
                     theta = 0.85,
                     varnames = list(ALEAF = "Photo",  #umol m-2 s-1
                                     Tleaf = "Tleaf", #Celsius
                                     Ci = "Ci", #umol mol-1
                                     PPFD = "PARi", #umol m-2 s-1
                                     Rd = "Rd", #umol m-2 s-1
                                     Press = "Press")){ #kPa
  #Assign group names and pressure
  data$group1 <- data[,group1]
  data$Press <- data[,varnames$Press]
  
  if(!is.na(group2)){
  data$group2 <- data[,group2]
  }
  
  if(!is.na(group3)){
  data$group3 <- data[,group3]
  }
  
  if(!is.na(group2) & !is.na(group3)){
  data <- unite(data, col = "group",
                c("group1", "group2", "group3"),
                sep = "_")
  } else{
    if(!is.na(group2) & is.na(group3)){
      data <- unite(data, col = "group",
                    c("group1", "group2"),
                    sep = "_")
    } else {
      data$group <- data$group1
    }
  }
  
  #Split data by group
  data <- split(data, data$group)
  
  #Create empty list for curve fits.
  fits <- as.list(1:length(data))
  
  #Fit ACi curves
  for (i in 1:length(data)){
    #Calculate mesophyll conductance
    gmeso <- gm25 * exp(Egm * 
                          (mean(data[[i]]$Tleaf + 
                                  273.15) - 298.15) /
                          (298.15 * 
                             mean(data[[i]]$Tleaf + 273.15) *
                             0.008314))
    
    #Calculate Km
    Km <- K25 * exp(Ek * 
                      (mean(data[[i]]$Tleaf + 
                              273.15) - 298.15) /
                      (298.15 * 
                         mean(data[[i]]$Tleaf + 273.15) *
                         0.008314))
    Patm <- mean(data[[i]]$Press)
    
    #Calculate GammaStar
    GammaStar <- Gstar25 * exp(Egamma * 
                      (mean(data[[i]]$Tleaf + 
                              273.15) - 298.15) /
                      (298.15 * 
                         mean(data[[i]]$Tleaf + 273.15) *
                         0.008314))

    #Fit ACi curve
    fits[[i]] <- fitaci(data[[i]], 
                        Patm = Patm,
                        varnames = varnames,
                        fitmethod = fitmethod, 
                        Tcorrect = Tcorrect,
                        fitTPU = fitTPU,
                        gmeso = gmeso, 
                        Km = Km,
                        GammaStar = GammaStar,
                        useRd = useRd,
                        citransition = citransition,
                        alphag = alphag,
                        PPFD = PPFD,
                        Tleaf = Tleaf,
                        alpha = alpha,
                        theta = theta)
    
    #Assign names
    names(fits)[i] <- data[[i]]$group[1]
  }
  
  #Return curve fits in list
  return(fits)
}

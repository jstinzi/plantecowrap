#' Get temperature response parameters
#'
#' @param data List of data output from fit_topt_VJs
#'
#' @return get_t_pars returns temperature response parameters for
#' Vcmax and Jmax from the group fitting process
#' @export
#' @examples \dontrun{
#' data <- read.csv("AllMyACiCurves.csv")
#' 
#' fits <- fitacis2(data,
#'                  group1 = "COLUMNNAME",
#'                  gm25 = 10000,#ensures curve is fit as ACi
#'                  Egm = 0,#ensures curve is fit as ACi
#'                  K25 = 718.4,#tobacco value
#'                  Ek = 65.60828,#tobacco value
#'                  Gstar = 42.75,#tobacco value
#'                  Egamma = 37.83,#tobacco value
#'                  fitmethod = "bilinear",
#'                  fitTPU = TRUE,#may need to set to false
#'                  varnames = list(ALEAF = "Photo",#name of Anet column needed in quotes
#'                                  Tleaf = "Tleaf",#name of Tleaf column needed in quotes
#'                                  Ci = "Ci",#name of intercellular CO2 column needed in quotes
#'                                  PPFD = "PARi",#name of light intensity column needed in quotes
#'                                  Rd = "Rd")#unless useRd = TRUE, this part is ignored
#' )
#' 
#' #Plot curve fits
#' for (i in 1:length(fits)){
#'   plot(fits[[i]])
#' }
#' 
#' #Obtain ACi parameter information for further processing and analysis
#' outputs <- acisummary(data,
#'                       group1 = "COLUMNNAME",
#'                       fits)
#' 
#' #Fit the Topt model from Medlyn et al. 2002 for all individuals
#' #Output is a list of lists for each individual
#' #There is also a fit_topt_VJ for single temperature response
#' #fitting
#' out <- fit_topt_VJs(data = outputs,
#'                     group = "Plant",#this grouping variable is for each individual
#'                     varnames = list(Vcmax = "Vcmax",
#'                                     Jmax = "Jmax",
#'                                     Tleaf = "Tleaf"),
#'                     limit_jmax = 100000,
#'                     limit_vcmax = 100000)
#' 
#' #Let's get the parameters out into a single data frame
#' pars <- get_t_pars(out)
#' }
get_t_pars <- function(data){
  pars <- list()
  for(i in 1:length(data)){
    pars[[i]] <- data[[i]][[1]]
    pars[[i]]$ID <- names(data[i])
  }
  pars <- do.call("rbind", pars)
  return(pars)
}

#' Fitting multiple temperature response curves
#'
#' @param data Dataframe with multiple temperature response curves
#' @param group Grouping variable to use
#' @param varnames Variable names.
#' @param limit_jmax Upper limit to Jmax values for fitting
#' @param limit_vcmax Upper limit to Vcmax values for fitting
#'
#' @return fit_topt_VJs fits multiple Vcmax and Jmax temperature responses
#' using the optimum temperature response model from Medlyn et al. 2002.
#' 
#' REFERENCE
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
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
#' 
#' #Let's get the graphs out into a list
#' #You can few a graph using: graph[1]
#' graphs <- get_t_graphs(out)
#' 
#' #Print graphs out as jpegs into folder
#' print_graphs(graphs)
#' }
fit_topt_VJs <- function(data,
                         group,
                         varnames = list(Vcmax = "Vcmax",
                                         Jmax = "Jmax",
                                         Tleaf = "Tleaf"),
                         limit_jmax = 100000,
                         limit_vcmax = 100000){
  #Assign group name
  data$group <- data[, group]
  
  #Split by group
  data <- split(data, data$group)
  
  #Create output list
  fits <- list()
  
  #Fit temperature response model
  for(i in 1:length(data)){
    fits[[i]] <- fit_topt_VJ(data = data[[i]],
                           varnames = varnames,
                           title = names(data[i]),
                           limit_jmax = limit_jmax,
                           limit_vcmax = limit_vcmax)
    
    #Assign names
    names(fits)[i] <- names(data[i])
  }
  
  #Return curve fits
  return(fits)
}

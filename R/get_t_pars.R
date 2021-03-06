#' Get temperature response parameters
#'
#' @param data List of data output from fit_topt_VJs
#' @return get_t_pars returns temperature response parameters for
#' Vcmax and Jmax from the group fitting process. Output is a
#' dataframe.
#' @export
#' @examples \donttest{
#' #Read in data
#' data <- read.csv(system.file("extdata", "example_2.csv",
#' package = "plantecowrap"), stringsAsFactors = FALSE)
#' #Fit ACi Curves then fit temperature responses
#' fits <- fitacis2(data = data,
#'                  varnames = list(ALEAF = "A",
#'                                  Tleaf = "Tleaf",
#'                                  Ci = "Ci",
#'                                  PPFD = "PPFD",
#'                                  Rd = "Rd",
#'                                  Press = "Press"),
#'                  group1 = "Grouping",
#'                  fitTPU = FALSE,
#'                  fitmethod = "bilinear",
#'                  gm25 = 10000,
#'                  Egm = 0)
#' #Extract coefficients
#' outputs <- acisummary(data, group1 = "Grouping", fits = fits)
#' #Plot curve fits
#' for (i in 1:length(fits)) {
#'   plot(fits[[i]])
#' }
#' #Separate out grouping variable
#' outputs <- separate(outputs, col = "ID", c("Treat", "Block"), sep = "_")
#' #Fit the Topt model from Medlyn et al. 2002 for all individuals
#' #Output is a list of lists for each individual
#' #There is also a fit_topt_VJ for single temperature response
#' #fitting
#' out <- fit_topt_VJs(data = outputs,
#'                     group = "Block", #this grouping variable is for
#'                     #each individual
#'                     varnames = list(Vcmax = "Vcmax",
#'                                     Jmax = "Jmax",
#'                                     Tleaf = "Tleaf"),
#'                     limit_jmax = 100000,
#'                     limit_vcmax = 100000)
#' #Let's get the parameters out into a single data frame
#' pars <- get_t_pars(out)
#' }
get_t_pars <- function(data) {
  #Create empty list
  pars <- list()
  #Add parameters to list
  for (i in 1:length(data)) {
    pars[[i]] <- data[[i]][[1]]
    pars[[i]]$ID <- names(data[i])
  }
  #Convert list to dataframe
  pars <- do.call("rbind", pars)
  #Return dataframe
  return(pars)
}

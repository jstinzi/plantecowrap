#' Get temperature response graphs
#'
#' @param data List of data output from fit_topt_VJs
#' @return get_t_graphs returns temperature response graphs for
#' Vcmax and Jmax from the group fitting process. Output is a
#' list of graphs.
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
#' #Let's get the graphs out into a list
#' #You can get a graph using: graph[1]
#' graphs <- get_t_graphs(out)
#' }
get_t_graphs <- function(data) {
  #Create empty list
  graphs <- list()
  #Add plots to list
  for (i in 1:length(data)) {
    graphs[[i]] <- data[[i]][3]
    names(graphs[[i]]) <- names(data[i])
  }
  #Return list
  return(graphs)
}

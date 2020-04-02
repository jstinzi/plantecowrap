#' Printing graphs from a list of graphs
#'
#' @param data List of graphs to output as .jpeg files
#' @param path File path for printing out graphs. Use "./" to set to current
#' working directory.
#' @param height Height of output graphs. Defaults to 5.
#' @param width Width of output graphs. Defaults to 5.
#' @param res Resolution of output graphs. Defaults to 600.
#' @param units Units of height and width. Defaults to "in".
#' @param ... Further arguments, specifically for jpeg().
#' @return print_graphs creates jpeg files from a list of graphs based on
#' the graph names. Used in combination with get_t_graphs. Output is a
#' series of .jpeg files in the working directory.
#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom utils prompt
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
#' #Print graphs out as jpegs into folder
#' print_graphs(graphs, path = tempdir())
#' }
print_graphs <- function(data,
                         path,# = "./",
                         height = 5,
                         width = 5,
                         res = 600,
                         units = "in",
                         ...) {
  if (!missing(path)) {
  #Print out each graph in the list to 'path'
  for (i in 1:length(data)) {
    jpeg(file.path(path, paste0(names(data[[i]])[1], ".jpeg")),
         height = height, width = width, res = res,
         units = units,
         ...)
    print(data[[i]])
    dev.off()
  }
  } else {
    print("Graphs not printed. 'path' argument required.")
  }
}

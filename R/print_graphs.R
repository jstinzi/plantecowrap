#' Printing graphs from a list of graphs
#'
#' @param data List of graphs to output as .jpeg files
#'
#' @return print_graphs creates jpeg files from a list of graphs based on
#' the graph names
#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @export
print_graphs <- function(data){
  for(i in 1:length(data)){
    jpeg(paste(names(data[[i]])[1], ".jpeg"), height = 5, width = 5, res = 600, units = "in")
    print(data[[i]])
    dev.off()
  }
}

#' Get temperature response graphs
#'
#' @param data List of data output from fit_topt_VJs
#'
#' @return get_t_pars returns temperature response graphs for
#' Vcmax and Jmax from the group fitting process
#' @export
get_t_graphs <- function(data){
  graphs <- list()
  for(i in 1:length(data)){
    graphs[[i]] <- data[[i]][3]
    names(graphs[[i]]) <- names(data[i])
  }
  return(graphs)
}

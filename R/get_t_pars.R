#' Get temperature response parameters
#'
#' @param data List of data output from fit_topt_VJs
#'
#' @return get_t_pars returns temperature response parameters for
#' Vcmax and Jmax from the group fitting process
#' @export
get_t_pars <- function(data){
  pars <- list()
  for(i in 1:length(data)){
    pars[[i]] <- data[[i]][[1]]
    pars[[i]]$ID <- names(data[i])
  }
  pars <- do.call("rbind", pars)
  return(pars)
}

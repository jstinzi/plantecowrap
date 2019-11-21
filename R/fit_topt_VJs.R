#' Fitting multiple temperature response curves
#'
#' @param data Dataframe with multiple temperature response curves
#' @param group Grouping variable to use
#' @param varnames Variable names
#' @param limit_jmax Upper limit to Jmax values for fitting
#' @param limit_vcmax Upper limit to Vcmax values for fitting
#'
#' @return fit_topt_VJs fits multiple Vcmax and Jmax temperature responses
#' @export
fit_topt_VJs <- function(data,
                         group,
                         varnames = list(Vcmax = "Vcmax",
                                         Jmax = "Jmax",
                                         Tleaf = "Tleaf"),
                         limit_jmax = 100000,
                         limit_vcmax = 100000){
  data$group <- data[, group]
  data <- split(data, data$group)
  
  fits <- list()
  
  for(i in 1:length(data)){
    fits[[i]] <- fit_topt_VJ(data = data[[i]],
                           varnames = varnames,
                           title = names(data[i]),
                           limit_jmax = limit_jmax,
                           limit_vcmax = limit_vcmax)
    names(fits)[i] <- names(data[i])
  }
  return(fits)
}

#' Fitting multiple temperature response curves
#'
#' @param data Dataframe with multiple temperature response curves
#' @param group Grouping variable to use
#' @param varnames Variable names
#'
#' @return fit_topt_VJs fits multiple Vcmax and Jmax temperature responses
#' @export
fit_topt_VJs <- function(data,
                         group,
                         varnames = list(Vcmax = "Vcmax",
                                         Jmax = "Jmax",
                                         Tleaf = "Tleaf")){
  data$group <- data[, group]
  data <- split(data, data$group)
  
  fits <- list()
  
  for(i in 1:length(data)){
    fits[[i]] <- fit_topt_VJ(data = data[[i]],
                           varnames = varnames,
                           title = names(data[i]))
    names(fits)[i] <- names(data[i])
  }
  return(fits)
}

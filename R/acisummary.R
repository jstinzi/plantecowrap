#' Extracts coefficients from fitacis2
#'
#' @param data data frame with A/Ci curve data
#' @param group1 grouping variable 1, must match fitacis2
#' @param group2 grouping variable 2, must match fitacis2
#' @param group3 grouping variable 3, must match fitacis2
#' @param fits list output from fitacis2
#'
#' @return acisummary produces a data frame with A-Ci coefficients
#' @importFrom tidyr unite
#' @importFrom stats coef
#' @export
#' @examples \dontrun{
#' #Read in data
#' data <- read.csv("mydata.csv")
#' #Run ACi curve fitting
#' fits <- fitacis2(data, group1 = "a", group2 = "b", group3 = "c",
#' fitmethod = "bilinear", fitTPU = TRUE, Tcorrect = FALSE)
#' #Extract coefficients
#' outputs <- acisummary(data, group1 = "a", group2 = "b", group3 = "c", fits)
#' }
#'
acisummary <- function(data,
                       group1,
                       group2 = NA,
                       group3 = NA,
                       fits) {
  #Create a dataframe for outputs
  data_output <- as.data.frame(1:length(fits))
  colnames(data_output) <- "ID"
  #Prepare groups to extract variables by group
  data$group1 <- data[, group1]
  #Double check if group2 is available
  if (!is.na(group2)) {
    data$group2 <- data[, group2]
  }
  #Double check if group3 is available
  if (!is.na(group3)) {
    data$group3 <- data[, group3]
  }
  #Assign group based on which groupings are used
  if (!is.na(group2) & !is.na(group3)) {
    data <- unite(data, col = "group",
                  c("group1", "group2", "group3"),
                  sep = "_")
  } else {
    if (!is.na(group2) & is.na(group3)) {
      data <- unite(data, col = "group",
                    c("group1", "group2"),
                    sep = "_")
    } else {
      data$group <- data$group1
    }
  }
  #Split dataframe for list for ease of extraction
  data <- split(data, data$group)
  #Extract variables from curve fit
  for (i in 1:length(fits)) {
    data_output$ID[i] <- names(fits)[i]
    #Leaf temperature in Celsius
    data_output$Tleaf[i] <- mean(data[[i]]$Tleaf)
    #Atmospheric pressure in kPa
    data_output$Patm[i] <- mean(fits[[i]]$df$Patm)
    #Vcmax in umol m-2 s-1
    data_output$Vcmax[i] <- coef(fits[[i]])[1]
    #Jmax in umol m-2 s-1
    data_output$Jmax[i] <- coef(fits[[i]])[2]
    #TPU in umol m-2 s-1
    data_output$TPU[i] <- coef(fits[[i]])[4]
    #Rd in umol m-2 s-1
    data_output$Rd[i] <- coef(fits[[i]])[3]
    #Vcmax/Jmax Ci transition point in umol mol-1
    data_output$Citrans1[i] <- fits[[i]]$Ci_transition
    #Jmax/TPU Ci transition point in umol mol-1
    data_output$Citrans2[i] <- fits[[i]]$Ci_transition2
    #Km in umol mol-1
    data_output$Km[i] <- fits[[i]]$Km
    #GammaStar in umol mol-1
    data_output$GammaStar[i] <- fits[[i]]$GammaStar
    #gmeso in mol m-2 s-1 bar-1
    data_output$gmeso[i] <- fits[[i]]$gmeso
  }
  #Produce output
  return(data_output)
}

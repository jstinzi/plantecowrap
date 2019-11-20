#' Extracts coefficients from fitacis2
#'
#' @param data data frame with A/Ci curves
#' @param group1 grouping variable 1, must match fitacis2
#' @param group2 grouping variable 2, must match fitacis2
#' @param group3 grouping variable 3, must match fitacis2
#' @param fits list output from fitacis2
#'
#' @return acisummary produces a data frame with A-Ci coefficients
#' @importFrom tidyr unite
#' @importFrom stats coef
#' @export
#' 
#' @examples 
#' \dontrun{
#' data <- read.csv("mydata.csv")
#' fits <- fitacis2(data, group1 = "a", group2 = "b", group3 = "c",
#' fitmethod = "bilinear", fitTPU = TRUE, Tcorrect = FALSE)
#' outputs <- acisummary(data, group1 = "a", group2 = "b", group3 = "c", fits)
#' }
#'
acisummary <- function(data,
                       group1,
                       group2,
                       group3,
                       fits){
  #Create a dataframe for outputs
  data_output <- as.data.frame(1:length(fits))
  colnames(data_output) <- "ID"
  #Prepare groups to extract variables by group
  data$group1 <- data[,group1]
  data$group2 <- data[,group2]
  data$group3 <- data[,group3]
  #Create one grouping variable
  data <- unite(data, col = "group",
                c("group1", "group2", "group3"),
                sep = "_")
  #Split dataframe for list for ease of extraction
  data <- split(data, data$group)
  #Extract variables from curve fit
  for(i in 1:length(fits)){
    data_output$ID[i] <- names(fits)[i]
    data_output$Tleaf[i] <- mean(data[[i]]$Tleaf)
    data_output$Patm[i] <- mean(fits[[i]]$df$Patm)
    data_output$Vcmax[i] <- coef(fits[[i]])[1]
    data_output$Jmax[i] <- coef(fits[[i]])[2]
    data_output$TPU[i] <- coef(fits[[i]])[4]
    data_output$Rd[i] <- coef(fits[[i]])[3]
    data_output$Citrans1[i] <- fits[[i]]$Ci_transition
    data_output$Citrans2[i] <- fits[[i]]$Ci_transition2
    data_output$Km[i] <- fits[[i]]$Km
    data_output$GammaStar[i] <- fits[[i]]$GammaStar
    data_output$gmeso[i] <- fits[[i]]$gmeso
  }
  #Produce output
  return(data_output)
}
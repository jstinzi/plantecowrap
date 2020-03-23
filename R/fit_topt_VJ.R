#' Fitting the temperature responses of Vcmax and Jmax
#'
#' @param data Dataframe containing Vcmax (maximum rubisco carboxylation
#' capacity in umol m-2 s-1), Jmax (maximum photosynthetic electron transport
#' to CO2 fixation in umol m-2 s-1), and Tleaf (leaf temperature in Celsius)
#' @param varnames Variable names to account for different spellings of Vcmax,
#' Jmax, and Tleaf.
#' @param title Graph title, usually a group name
#' @param limit_jmax Upper limit to Jmax values for fitting. Defaults to
#' 100,000 umol m-2 s-1 as this is the "nonsense output" from fitaci. Ensures
#' that these points are not fit.
#' @param limit_vcmax Upper limit to Vcmax values for fitting. Defaults to
#' 100,000 umol m-2 s-1.
#'
#' @return fit_topt_VJ fits the Topt modified Arrhenius function to Vcmax and
#' Jmax data. Note that Hd may max out at 3000 kJ mol-1 for Jmax and 2000 kJ
#' mol-1 for Vcmax.
#' 
#' REFERENCE
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom minpack.lm nlsLM
#' @importFrom stats BIC
#' @importFrom stats coef
#' @importFrom stats nls.control
#' @export
#' 
#' @examples \dontrun{
#' #Create Example Dataset
#' 
#' Tleaf <- seq(from = 10, to = 35, by = 5)
#' Tleaf <- sort(rep(Tleaf, 15))
#' 
#' Vcmax_start <- 90 * modarrhenius(Ea = 35, Hd = 220, dS = 0.3, Tleaf = Tleaf)
#' Jmax_start <- 140 * modarrhenius(Ea = 80, Hd = 220, dS = 0.6, Tleaf = Tleaf)
#' R_start <- 1.5 * arrhenius(Ea = 20, Tleaf = Tleaf)
#' GammaStar_start <- 42.75 * arrhenius(Ea = 37.83, Tleaf = Tleaf)
#' Km_start <- 718.40 * arrhenius(Ea = 65.50828, Tleaf = Tleaf)
#' Ci <- rep(c(75, 100, 150, 200, 250,
#'             300, 350, 400, 600, 800,
#'             1000, 1200, 1400, 1600, 1800), 6)
#' A <- 1:90
#' Vc <- 1:90
#' Vj <- 1:90
#' for(i in seq_along(A)){
#'   Vc[i] <- (Vcmax_start[i] * (Ci[i] - GammaStar_start[i]) / 
#'               (Ci[i] + Km_start[i]))
#'   Vj[i] <- (Jmax_start[i] * (Ci[i] - GammaStar_start[i]) / 
#'               (4 * Ci[i] + 8 * GammaStar_start[i]))
#'   if(Vc[i] < 0){
#'     Vc[i] <- NA
#'   }
#'   if(Vj[i] < 0){
#'     Vj[i] <- NA
#'   }
#'   A[i] <- min(Vc[i], Vj[i]) - R_start[i]
#' }
#' 
#' PPFD <- rep(2000, 90)
#' Press <- rep(100, 90)
#' 
#' data <- data.frame(cbind(A,
#'                          Ci,
#'                          Tleaf,
#'                          Press,
#'                          PPFD))
#' 
#' data$Treat <- as.character(Tleaf)
#' 
#' 
#' #Fit ACi Curves then fit temperature responses
#' fits <- fitacis2(data = data,
#'                  varnames = list(ALEAF = "A",
#'                                  Tleaf = "Tleaf",
#'                                  Ci = "Ci",
#'                                  PPFD = "PPFD",
#'                                  Rd = "Rd",
#'                                  Press = "Press"),
#'                  group1 = "Treat",
#'                  fitTPU = FALSE,
#'                  fitmethod = "bilinear",
#'                  gm25 = 10000,
#'                  Egm = 0)
#' 
#' #Extract coefficients
#' outputs <- acisummary(data, group1 = "Treat", fits = fits)
#' 
#' #Fit temperature response
#' tresp <- fit_topt_VJ(outputs)
#' 
#' #View plot
#' tresp[[3]]
#' }
fit_topt_VJ <- function(data,
                           varnames = list(Vcmax = "Vcmax",
                                           Jmax = "Jmax",
                                           Tleaf = "Tleaf"),
                        title = NULL,
                        limit_jmax = 100000,
                        limit_vcmax = 100000){
  #Define variables locally to avoid binding issue
  Jmax <- NULL
  Tleaf <- NULL
  Vcmax <- NULL
  
  #Assign variable names
  data$Tleaf <- data[, varnames$Tleaf]
  data$Vcmax <- data[, varnames$Vcmax]
  data$Jmax <- data[, varnames$Jmax]
  
  #Remove points that are nonsensical
  data$Jmax <- ifelse(data$Jmax < limit_jmax, data$Jmax, NA)
  data$Vcmax <- ifelse(data$Vcmax < limit_vcmax, data$Vcmax, NA)
  
  #Create output list
  outputs <- list()
  
  #Create output dataframe
  arrhenius_out <- as.data.frame(cbind(rep(0, 2),#Parameter
                                       rep(0, 2),#v25
                                       rep(0, 2),#Ea
                                       rep(0, 2),#Residual
                                       rep(0, 2)#BIC
                                       ))
  
  #Add column names
  colnames(arrhenius_out) <- c("Parameter", "k25", "Ea", "Residual", "BIC")
  
  #Basically, use Arrhenius curve to feed Ea into Topt function start
  #Try approach where you start Hd from 1 to 1000
  #select minimum residual
  Vcmax_m <- nlsLM(data = data[!is.na(data$Vcmax), ], 
                   Vcmax ~ v25 * arrhenius(Ea,
                                           Tleaf = Tleaf),
                   start = list(v25 = 1, Ea = 1),
                   control = nls.control(maxiter = 100)
  )
  
  #Extract coefficients
  arrhenius_out$Parameter[1] <- "Vcmax"
  arrhenius_out$k25[1] <- coef(Vcmax_m)[1]
  arrhenius_out$Ea[1] <- coef(Vcmax_m)[2]
  arrhenius_out$Residual[1] <- sum(abs(Vcmax_m$m$resid()))
  arrhenius_out$BIC[1] <- BIC(Vcmax_m)
  
  #Create dataframe for iterative model fits
  Vcmax_fm <- as.data.frame(cbind(rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep("Vcmax", 1000)))
  
  #Add column names
  colnames(Vcmax_fm) <- c("Ea", "Hd", "kopt", "Topt", "residual", "BIC", "Parameter")
  
  #Set variable classes
  Vcmax_fm$Ea <- as.double(Vcmax_fm$Ea)
  Vcmax_fm$Hd <- as.double(Vcmax_fm$Hd)
  Vcmax_fm$kopt <- as.double(Vcmax_fm$kopt)
  Vcmax_fm$Topt <- as.double(Vcmax_fm$Topt)
  Vcmax_fm$residual <- as.double(Vcmax_fm$residual)
  Vcmax_fm$BIC <- as.double(Vcmax_fm$BIC)
  
  #Run model fit loop
  for(i in 1:1000){
    model <- NULL
    model <- tryCatch(nlsLM(data = data[!is.na(data$Vcmax), ], 
                            Vcmax ~ toptfit(Ea,
                                            Hd,
                                            kopt,
                                            Topt,
                                            Tleaf = Tleaf),
                            start = list(Ea = coef(Vcmax_m)[[2]],
                                         Hd = i,
                                         kopt = max(data[!is.na(data$Vcmax), ]$Vcmax),
                                         Topt = max(data[!is.na(data$Vcmax), ]$Tleaf)),
                            lower = c(0, 0, 0, 0),
                            upper = c(1000, 2000, max(data[!is.na(data$Vcmax), ]$Vcmax) + 1,
                                      max(data[!is.na(data$Vcmax), ]$Tleaf) + 1),
                            control = nls.control(maxiter = 100)),
                      error = function(e) paste(NA)
    )
    Vcmax_fm$Ea[i] <- tryCatch(coef(model)[[1]],
                               error = function(e) paste(NA))
    Vcmax_fm$Hd[i] = tryCatch(coef(model)[[2]],
                              error = function(e) paste(NA))
    Vcmax_fm$kopt[i] = tryCatch(coef(model)[[3]],
                                error = function(e) paste(NA))
    Vcmax_fm$Topt[i] = tryCatch(coef(model)[[4]],
                                error = function(e) paste(NA))
    Vcmax_fm$residual[i] <- tryCatch(sum(abs(model$m$resid())),
                                     error = function(e) paste(NA))
    Vcmax_fm$BIC[i] <- tryCatch(BIC(model),
                                     error = function(e) paste(NA))
  }
  
  #Ensure classes are correct
  Vcmax_fm$Ea <- as.double(Vcmax_fm$Ea)
  Vcmax_fm$Hd <- as.double(Vcmax_fm$Hd)
  Vcmax_fm$kopt <- as.double(Vcmax_fm$kopt)
  Vcmax_fm$Topt <- as.double(Vcmax_fm$Topt)
  Vcmax_fm$residual <- as.double(Vcmax_fm$residual)
  Vcmax_fm$BIC <- as.double(Vcmax_fm$BIC)
  
  #Narrow down fits
  Vcmax_fm <- Vcmax_fm[is.na(Vcmax_fm$residual) == FALSE, ]
  
  #Select best fit
  Vcmax_fm <- Vcmax_fm[Vcmax_fm$residual == min(Vcmax_fm$residual), ]
  
  #Create modelled data for graph
  T_model <- seq(from = min(data$Tleaf),
                 to = max(data$Tleaf),
                 length.out = length(data$Tleaf))
  Vcmax_pred <- toptfit(Ea = Vcmax_fm$Ea[1],
                        Hd = Vcmax_fm$Hd[1],
                        kopt = Vcmax_fm$kopt[1],
                        Topt = Vcmax_fm$Topt[1],
                        Tleaf = T_model)
  
  ##END VCMAX
  
  #Get initial parameters for Jmax
  Jmax_m <- nlsLM(data = data[!is.na(data$Jmax), ], 
                  Jmax ~ j25 * arrhenius(Ea,
                                         Tleaf = Tleaf),
                  start = list(j25 = 1, Ea = 1),
                  control = nls.control(maxiter = 100)
  )
  
  #Extract parameters
  arrhenius_out$Parameter[2] <- "Jmax"
  arrhenius_out$k25[2] <- coef(Jmax_m)[1]
  arrhenius_out$Ea[2] <- coef(Jmax_m)[2]
  arrhenius_out$Residual[2] <- sum(abs(Jmax_m$m$resid()))
  arrhenius_out$BIC[2] <- BIC(Jmax_m)
  
  #Setup iteration dataframe
  Jmax_fm <- as.data.frame(cbind(rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep("Jmax", 1000)))
  
  #Add column names
  colnames(Jmax_fm) <- c("Ea", "Hd", "kopt", "Topt", "residual", "BIC", "Parameter")
  
  #Set variable classes
  Jmax_fm$Ea <- as.double(Jmax_fm$Ea)
  Jmax_fm$Hd <- as.double(Jmax_fm$Hd)
  Jmax_fm$kopt <- as.double(Jmax_fm$kopt)
  Jmax_fm$Topt <- as.double(Jmax_fm$Topt)
  Jmax_fm$residual <- as.double(Jmax_fm$residual)
  Jmax_fm$BIC <- as.double(Jmax_fm$BIC)
  
  #Start iteration
  for(i in 1:1000){
    model <- NULL
    model <- tryCatch(nlsLM(data = data[!is.na(data$Jmax), ], 
                            Jmax ~ toptfit(Ea,
                                           Hd,
                                           kopt,
                                           Topt,
                                           Tleaf = Tleaf),
                            start = list(Ea = coef(Jmax_m)[[2]],
                                         Hd = i,
                                         kopt = max(data[!is.na(data$Jmax), ]$Jmax),
                                         Topt = max(data[!is.na(data$Jmax), ]$Tleaf)),
                            lower = c(0, 0, 0, 0),
                            upper = c(1000, 3000, max(data[!is.na(data$Jmax), ]$Jmax) + 1,
                                      max(data[!is.na(data$Jmax), ]$Tleaf) + 1),
                            control = nls.control(maxiter = 100)),
                      error = function(e) paste(NA)
    )
    Jmax_fm$Ea[i] <- tryCatch(coef(model)[[1]],
                              error = function(e) paste(NA))
    Jmax_fm$Hd[i] = tryCatch(coef(model)[[2]],
                             error = function(e) paste(NA))
    Jmax_fm$kopt[i] = tryCatch(coef(model)[[3]],
                               error = function(e) paste(NA))
    Jmax_fm$Topt[i] = tryCatch(coef(model)[[4]],
                               error = function(e) paste(NA))
    Jmax_fm$residual[i] <- tryCatch(sum(abs(model$m$resid())),
                                    error = function(e) paste(NA))
    Jmax_fm$BIC[i] <- tryCatch(BIC(model),
                                   error = function(e) paste(NA))
  }
  
  #Ensure variable classes
  Jmax_fm$Ea <- as.double(Jmax_fm$Ea)
  Jmax_fm$Hd <- as.double(Jmax_fm$Hd)
  Jmax_fm$kopt <- as.double(Jmax_fm$kopt)
  Jmax_fm$Topt <- as.double(Jmax_fm$Topt)
  Jmax_fm$residual <- as.double(Jmax_fm$residual)
  Jmax_fm$BIC <- as.double(Jmax_fm$BIC)
  
  #Narrow down fits
  Jmax_fm <- Jmax_fm[is.na(Jmax_fm$residual) == FALSE, ]
  
  #Select best fit
  Jmax_fm <- Jmax_fm[Jmax_fm$residual == min(Jmax_fm$residual), ]
  
  #Model data for graphing
  T_model <- seq(from = min(data$Tleaf),
                 to = max(data$Tleaf),
                 length.out = length(data$Tleaf))
  Jmax_pred <- toptfit(Ea = Jmax_fm$Ea[1],
                       Hd = Jmax_fm$Hd[1],
                       kopt = Jmax_fm$kopt[1],
                       Topt = Jmax_fm$Topt[1],
                       Tleaf = T_model)
  
  #Assign outputs
  outputs[[1]] <- rbind(Vcmax_fm, Jmax_fm)
  
  outputs[[2]] <- arrhenius_out
  
  outputs[[3]] <- ggplot(data, aes(x = Tleaf, y = Vcmax))+
    ggtitle(label = title)+
    labs(x = expression("Tleaf (Celsius)"),
         y = expression("Jmax or Vcmax ("*mu*mol~m^{-2}~s^{-1}*")"))+
    geom_smooth(aes(y = Vcmax_pred, x = T_model, colour = "pink"),
                formula = y ~ toptfit(Ea = Vcmax_fm$Ea[1],
                                      Hd = Vcmax_fm$Hd[1],
                                      kopt = Vcmax_fm$kopt[1],
                                      Topt = Vcmax_fm$Topt[1],
                                      Tleaf = x),
                method = "lm",
                se = FALSE, size = 2)+
    geom_smooth(aes(y = Jmax_pred, x = T_model, colour = "cyan"),
                formula = y ~ toptfit(Ea = Jmax_fm$Ea[1],
                                      Hd = Jmax_fm$Hd[1],
                                      kopt = Jmax_fm$kopt[1],
                                      Topt = Jmax_fm$Topt[1],
                                      Tleaf = x),
                method = "lm",
                se = FALSE, size = 2)+
    geom_point(aes(fill = "pink"), colour = "black", shape = 21, size = 3)+
    geom_point(aes(y = Jmax, fill = "cyan"), colour = "black", shape = 21, size = 3)+
    scale_colour_manual(labels = c("Jmax_mod", "Vcmax_mod"),
                        values = c("red3", "dodgerblue3"))+
    scale_fill_manual(labels = c("Jmax", "Vcmax"),
                      values = c("red3", "dodgerblue3"))+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = c(0.15, 0.8),
          text = element_text(size = 14))
  names(outputs) <- c("Topt_Model", "Arrhenius_Model", "Graph")
  
  #Return outputs
  return(outputs)
}

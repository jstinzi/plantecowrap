#' Fitting the temperature responses of Vcmax and Jmax
#'
#' @param data Dataframe containing Vcmax, Jmax, and Tleaf
#' @param varnames Variable names
#' @param title Graph title, usually a group name
#'
#' @return fit_topt_VJ fits the Topt modified Arrhenius function to Vcmax and
#' Jmax data. Note that Hd may max out at 3000 kJ mol-1 for Jmax and 2000 for
#' Vcmax.
#' @importFrom minpack.lm nlsLM
#' @importFrom ggplot2 ggplot
#' @export
fit_topt_VJ <- function(data,
                           varnames = list(Vcmax = "Vcmax",
                                           Jmax = "Jmax",
                                           Tleaf = "Tleaf"),
                        title){
  data$Tleaf <- data[, varnames$Tleaf]
  data$Vcmax <- data[, varnames$Vcmax]
  data$Jmax <- data[, varnames$Jmax]
  
  outputs <- list()
  #Basically, use Arrhenius curve to feed Ea into Topt function start
  #Try approach where you start Hd from 1 to 1000
  #select minimum residual
  Vcmax_m <- nlsLM(data = data, 
                   Vcmax ~ v25 * arrhenius(Ea,
                                           Tleaf = Tleaf),
                   start = list(v25 = 1, Ea = 1),
                   control = nls.control(maxiter = 100)
  )
  
  Vcmax_fm <- as.data.frame(cbind(rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep(0, 1000),
                                  rep("Vcmax", 1000)))
  colnames(Vcmax_fm) <- c("Ea", "Hd", "kopt", "Topt", "residual", "Parameter")
  Vcmax_fm$Ea <- as.double(Vcmax_fm$Ea)
  Vcmax_fm$Hd <- as.double(Vcmax_fm$Hd)
  Vcmax_fm$kopt <- as.double(Vcmax_fm$kopt)
  Vcmax_fm$Topt <- as.double(Vcmax_fm$Topt)
  Vcmax_fm$residual <- as.double(Vcmax_fm$residual)
  for(i in 1:1000){
    model <- NULL
    model <- tryCatch(nlsLM(data = data, 
                            Vcmax ~ toptfit(Ea,
                                            Hd,
                                            kopt,
                                            Topt,
                                            Tleaf = Tleaf),
                            start = list(Ea = coef(Vcmax_m)[[2]],
                                         Hd = i,
                                         kopt = max(data$Vcmax),
                                         Topt = max(data$Tleaf)),
                            lower = c(0, 0, 0, 0),
                            upper = c(1000, 2000, max(data$Vcmax) + 1,
                                      max(data$Tleaf) + 1),
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
  }
  Vcmax_fm$Ea <- as.double(Vcmax_fm$Ea)
  Vcmax_fm$Hd <- as.double(Vcmax_fm$Hd)
  Vcmax_fm$kopt <- as.double(Vcmax_fm$kopt)
  Vcmax_fm$Topt <- as.double(Vcmax_fm$Topt)
  Vcmax_fm$residual <- as.double(Vcmax_fm$residual)
  Vcmax_fm <- Vcmax_fm[is.na(Vcmax_fm$residual) == FALSE, ]
  Vcmax_fm <- Vcmax_fm[Vcmax_fm$residual == min(Vcmax_fm$residual), ]
  T_model <- seq(from = min(data$Tleaf),
                 to = max(data$Tleaf),
                 length.out = length(data$Tleaf))
  Vcmax_pred <- toptfit(Ea = Vcmax_fm$Ea[1],
                        Hd = Vcmax_fm$Hd[1],
                        kopt = Vcmax_fm$kopt[1],
                        Topt = Vcmax_fm$Topt[1],
                        Tleaf = T_model)
  
  ##END VCMAX
  
  Jmax_m <- nlsLM(data = data, 
                  Jmax ~ j25 * arrhenius(Ea,
                                         Tleaf = Tleaf),
                  start = list(j25 = 1, Ea = 1),
                  control = nls.control(maxiter = 100)
  )
  
  Jmax_fm <- as.data.frame(cbind(rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep(0, 1000),
                                 rep("Jmax", 1000)))
  colnames(Jmax_fm) <- c("Ea", "Hd", "kopt", "Topt", "residual", "Parameter")
  Jmax_fm$Ea <- as.double(Jmax_fm$Ea)
  Jmax_fm$Hd <- as.double(Jmax_fm$Hd)
  Jmax_fm$kopt <- as.double(Jmax_fm$kopt)
  Jmax_fm$Topt <- as.double(Jmax_fm$Topt)
  Jmax_fm$residual <- as.double(Jmax_fm$residual)
  for(i in 1:1000){
    model <- NULL
    model <- tryCatch(nlsLM(data = data, 
                            Jmax ~ toptfit(Ea,
                                           Hd,
                                           kopt,
                                           Topt,
                                           Tleaf = Tleaf),
                            start = list(Ea = coef(Jmax_m)[[2]],
                                         Hd = i,
                                         kopt = max(data$Jmax),
                                         Topt = max(data$Tleaf)),
                            lower = c(0, 0, 0, 0),
                            upper = c(1000, 3000, max(data$Jmax) + 1,
                                      max(data$Tleaf) + 1),
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
  }
  Jmax_fm$Ea <- as.double(Jmax_fm$Ea)
  Jmax_fm$Hd <- as.double(Jmax_fm$Hd)
  Jmax_fm$kopt <- as.double(Jmax_fm$kopt)
  Jmax_fm$Topt <- as.double(Jmax_fm$Topt)
  Jmax_fm$residual <- as.double(Jmax_fm$residual)
  Jmax_fm <- Jmax_fm[is.na(Jmax_fm$residual) == FALSE, ]
  Jmax_fm <- Jmax_fm[Jmax_fm$residual == min(Jmax_fm$residual), ]
  T_model <- seq(from = min(data$Tleaf),
                 to = max(data$Tleaf),
                 length.out = length(data$Tleaf))
  Jmax_pred <- toptfit(Ea = Jmax_fm$Ea[1],
                       Hd = Jmax_fm$Hd[1],
                       kopt = Jmax_fm$kopt[1],
                       Topt = Jmax_fm$Topt[1],
                       Tleaf = T_model)
  
  outputs[[1]] <- rbind(Vcmax_fm, Jmax_fm)
  
  outputs[[2]] <- ggplot(data, aes(x = Tleaf, y = Vcmax))+
    ggtitle(label = title)+
    labs(x = expression("Tleaf (Celsius)"),
         y = expression("Jmax or Vcmax ("*mu*mol~m^{-2}~s^{-1}*")"))+
    geom_smooth(aes(y = Vcmax_pred, x = T_model, colour = "pink"),
                formula = y ~ toptfit(Ea = Vcmax_fm$Ea[1],
                                      Hd = Vcmax_fm$Hd[1],
                                      kopt = Vcmax_fm$kopt[1],
                                      Topt = Vcmax_fm$Topt[1],
                                      Tleaf = x),
                method = "loess",
                se = FALSE, size = 2)+
    geom_smooth(aes(y = Jmax_pred, x = T_model, colour = "cyan"),
                formula = y ~ toptfit(Ea = Jmax_fm$Ea[1],
                                      Hd = Jmax_fm$Hd[1],
                                      kopt = Jmax_fm$kopt[1],
                                      Topt = Jmax_fm$Topt[1],
                                      Tleaf = x),
                method = "loess",
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
  return(outputs)
}

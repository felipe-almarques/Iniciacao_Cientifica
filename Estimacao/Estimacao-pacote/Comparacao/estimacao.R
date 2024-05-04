estimacao <- function(n, M, garch_param, sv_param, gas_param) {
  # garch_param: list(alpha, beta)
  # sv_param: list(mu, phi, sigma)
  # gas_param: list(kappa, A, B, dist, scaling)
  
  
  
  ## Definindo os vetores de previsões
  volgarch_verdadeira <- rep(0, M) ; volgarch_garch <- rep(0, M)
  volgarch_sv <- rep(0, M) ; volgarch_gas <- rep(0, M)
  volsv_verdadeira <- rep(0, M) ; volsv_garch <- rep(0, M)
  volsv_sv <- rep(0, M) ; volsv_gas <- rep(0, M)
  volgas_verdadeira <- rep(0, M) ; volgas_garch <- rep(0, M)
  volgas_sv <- rep(0, M) ; volgas_gas <- rep(0, M)
  
  ## Definindo parametros
  alpha <- garch_param$alpha ; beta <- garch_param$beta
  #mu <- -10 ; phi <- .99 ; sigma <- .2
  mu <- sv_param$mu ; phi <- sv_param$phi ; sigma <- sv_param$sigma
  #kappa <- c(0, .1) ; A <- matrix(c(0, 0, 0, .3), ncol = 2) 
  #B <- matrix(c(0, 0, 0, .2), ncol = 2) ; dist <- "norm" ; Scaling <- "Identity"
  kappa <- gas_param$kappa ; A <- gas_param$A
  B <- gas_param$B ; dist <- gas_param$dist ; Scaling <- gas_param$scaling
  
  ## Definindo modelos
  spec_garch <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = F))
  spec_gas <- UniGASSpec(Dist = dist, ScalingType = Scaling, GASPar = list(scale = TRUE))
  
  for (t in 1:M) {
    message(paste0(t, " / ", M))
    ## Simulação
    amostra_garch <- garch.sim(alpha, beta, n + 1)
    amostra_sv <- svsim(n + 1, mu, phi, sigma)
    amostra_gas <- UniGASSim(T.sim = n + 1, kappa = kappa, A = A, B = B, Dist = dist, ScalingType = Scaling)
    
    ## Estimação
    # Garch
    fit_garch1 <- ugarchfit(spec_garch, amostra_garch[1:n])
    #fit_sv1 <- kalman_filter(amostra_garch[1:n])
    fit_gas1 <- UniGASFit(spec_gas, amostra_garch[1:n])
    # SV
    fit_garch2 <- ugarchfit(spec_garch, amostra_sv$y[1:n])
    #fit_sv2 <- kalman_filter(amostra_sv$y[1:n])
    fit_gas2 <- UniGASFit(spec_gas, amostra_sv$y[1:n])
    # Gas
    fit_garch3 <- ugarchfit(spec_garch, amostra_gas@Data$vY[1:n])
    #fit_sv3 <- kalman_filter(amostra_gas@Data$vY[1:n])
    fit_gas3 <- UniGASFit(spec_gas, amostra_gas@Data$vY[1:n])
    
    ## Previsão One-step-ahead
    # Garch
    fore_garch1 <- ugarchforecast(fit_garch1, n.ahead = 1)
    #fore_sv <- KalmanForecast()
    fore_gas1 <- UniGASFor(fit_gas1, H = 1)
    
    volgarch_verdadeira[t] <- amostra_garch[n+1]
    volgarch_garch[t] <- fore_garch1@forecast$sigmaFor
    volgarch_gas[t] <- fore_gas1@Forecast$PointForecast[2]
    
    # SV
    fore_garch2 <- ugarchforecast(fit_garch2, n.ahead = 1)
    #fore_sv <- KalmanForecast()
    fore_gas2 <- UniGASFor(fit_gas2, H = 1)
    
    volsv_verdadeira[t] <- amostra_sv$y[n+1]
    volsv_garch[t] <- fore_garch2@forecast$sigmaFor
    volgas_gas[t] <- fore_gas2@Forecast$PointForecast[2]
    
    # GAS
    fore_garch3 <- ugarchforecast(fit_garch3, n.ahead = 1)
    #fore_sv <- KalmanForecast()
    fore_gas3 <- UniGASFor(fit_gas3, H = 1)
    
    volgas_verdadeira[t] <- amostra_gas@Data$vY[n+1]
    volgas_garch[t] <- fore_garch2@forecast$sigmaFor
    volgas_gas[t] <- fore_gas2@Forecast$PointForecast[2]
  }
  
  dados <- data.frame(volgarch_verdadeira, volgarch_garch, volgarch_sv, volgarch_gas,
                      volsv_verdadeira, volsv_garch, volsv_sv, volsv_gas,
                      volgas_verdadeira, volgas_garch, volgas_sv, volgas_gas)
  
  return(dados)
}

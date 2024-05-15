# Função para estimação dos parâmetros de um SV via máxima verossimilhança construída através do filtro de kalman.
# A função está baseada no detalhamento teórico presente no artigo "A note on Stochastic Volatility models"
# m = 2 (duas normais na mistura)

# Params iniciais: (alpha, phi, sigmaw, sigma1, sigma2, mu1, mu2)

ll_sv <- function(params, r, fuller=FALSE, c=0.005) {
  # transformando os retornos
  if (fuller){
    s2 <- var(r)
    y <- log(r^2 + c*s2) - (c*s2 / (r^2 + c*s2))
  } else {
    y <- log(r^2)
  }
  
  # definindo variáveis e os vetores recursivos
  n <- length(y)
  loglik_sv <- 0
  h <- P <- rep(0, n+1)
  epsilon1 <- epsilon2 <- k1 <- k2 <- rep(0, n)
  Sigma1 <- Sigma2 <- pi1 <- pi2 <- rep(0, n) 

  # definindo os parâmetros
  alpha <- params[1]; phi <- params[2]; sigmaw <- params[3]
  sigma1 <- params[4]; sigma2 <- params[5]; mu1 <- params[6]
  mu2 <- params[7]

  # definindo valores iniciais
  P[1] <- phi^2 + sigmaw^2

  for (t in 1:n) {
    #message(paste("Iteração", t))
    epsilon1[t] <- y[t] - alpha - h[t] - mu1
    epsilon2[t] <- y[t] - alpha - h[t] - mu2
    #message(paste0("epsilon1[", t ,"] = ", epsilon1[t]))
    #message(paste0("epsilon2[", t ,"] = ", epsilon2[t]))
    Sigma1[t] <- P[t] + sigma1^2
    Sigma2[t] <- P[t] + sigma2^2
    #message(paste0("Sigma1[", t ,"] = ", Sigma1[t]))
    #message(paste0("Sigma2[", t ,"] = ", Sigma2[t]))
    k1[t] <- phi^2 * P[t] / Sigma1[t]
    k2[t] <- phi^2 * P[t] / Sigma2[t]
    #message(paste0("k1[", t ,"] = ", k1[t]))
    #message(paste0("k2[", t ,"] = ", k2[t]))
    pi1[t] <- (dnorm(y[t], h[t] + mu1, sqrt(Sigma1[t]))/2) / mean(c(dnorm(y[t], h[t] + mu1, sqrt(Sigma1[t])), dnorm(y[t], h[t] + mu2, sqrt(Sigma2[t]))))
    pi2[t] <- 1 - pi1[t]
    
    #message(paste0("pi1[", t ,"] = ", pi1[t]))
    #message(paste0("pi2[", t ,"] = ", pi2[t]))
    
    h[t+1] <- phi * h[t] + (pi1[t]*k1[t]*epsilon1[t]) + (pi2[t]*k2[t]*epsilon2[t])
    P[t+1] <- phi^2 * P[t] + sigmaw^2 - pi1[t]*k1[t]^2*epsilon1[t] - pi2[t]*k2[t]^2*epsilon2[t]
    
    #message(paste0("h[", t ,"] = ", h[t]))
    #message(paste0("P[", t ,"] = ", P[t]))
    
    loglik_sv <- loglik_sv + log((dnorm(y[t], h[t+1] + mu1, sqrt(Sigma1[t])) + dnorm(y[t], h[t+1] + mu1, sqrt(Sigma1[t])))/2) # conferir essa parte
    #message(loglik_sv)
  }
  #h <- h[2:n+1] ; P <- P[2:n+1]
  
  return(-loglik_sv)
}

estimacao_sv <- function(params, y, fuller=FALSE, c=0.005) {
  optim(params, ll_sv, r=y, fuller=fuller, c=c)
}

# ideia: não definir os parâmetros (usar no código "params[i]") para aumentar eficiencia



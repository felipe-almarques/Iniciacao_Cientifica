sv_sim <- function(n, beta, phi, sigma){ 
  # o modelo utilizado foi proposto por Taylor(1982, 1996)
  n_burnin <- 500
  epsilon <- rnorm(n + 1 + n_burnin)
  omega <- rnorm(n + 1 + n_burnin, mean = 0, sd = sigma)
  h = rep(0, n + 1 + n_burnin) # h[1] = 0 (Shumway and Stoffer)
  r = rep(0, n + 1 + n_burnin) 
  
  for (i in 2:(n + 1 + n_burnin)) {
    h[i] = phi*h[i - 1] + omega[i]
    r[i] = beta*exp(h[i]/2)*epsilon[i]
  }
  return(list(returns = r[-c(1:n_burnin)], 
              volatility = beta * exp(h[-c(1:n_burnin)]/2)))
}


dados <- sv_sim(2000,  0.016, 0.9, 0.67)
ts.plot(dados$returns)
ts.plot(dados$volatility)

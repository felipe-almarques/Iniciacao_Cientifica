sv_sim2 <- function(n, mu, phi, sigma){ 
  # o modelo utilizado foi proposto por Taylor(1982, 1996)
  n_burnin <- 500
  epsilon <- rnorm(n + 1 + n_burnin)
  omega <- rnorm(n + 1 + n_burnin, mean = 0, sd = sigma)
  h = rep(0, n + 1 + n_burnin) # h[1] = 0 (Shumway and Stoffer)
  r = rep(0, n + 1 + n_burnin)
  
  for (i in 2:(n + 1 + n_burnin)) {
    h[i] = mu + phi*(h[i-1] - mu) + omega[i]
    r[i] = exp(h[i]/2)*epsilon[i]  # acho que aqui é exp(h[i]) apenas, mas da uma conferida pfv
  }
  
  return(list(returns = r[-c(1:n_burnin)], 
              volatility = exp(h[-c(1:n_burnin)])))
}

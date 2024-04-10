garch_sim <- function(n, alpha, beta) { # Erros Normais
  # alpha[1] -> omega ; demais alphas compoem a somatoria dos retornos
  n_burnin <- 500
  order_max <- max(length(alpha) - 1, length(beta))
  epsilon <- rnorm(n + order_max + n_burnin)
  sigma2 <- rep(0, n + order_max + n_burnin)
  ret <- rep(0, n + order_max + n_burnin)
  
  for (i in (order_max + 1):(n + order_max + n_burnin)) {
    sum_alpha <- 0
    sum_beta <- 0
    for (p in 2:(length(alpha))) {sum_alpha <- sum_alpha + alpha[p]*(ret[i - p + 1])^2}
    for (q in 1:length(beta)) {sum_beta <- sum_beta + beta[q]*sigma2[i - q]}
    sigma2[i] <- alpha[1] + sum_alpha + sum_beta
    ret[i] <- sqrt(sigma2[i])*epsilon[i]
  }
  return(list(returns = ret[-c(1:n_burnin)], volatility = sqrt(sigma2[-c(1:n_burnin)])))
}

#dados <- garch_sim(2000, c(0.05, 0.05, 0.04), 0.85)

### Observacoes sobre a funcao
# k -> tamanho do vetor dos parametros que variam no tempo
# S -> matriz de cargas
# grad -> gradiente da log densidade
# mat_inf -> matriz informação (E[grad * grad'])


gas_sim <- function(omega, f="sigma", A, B, S = "Identity", n) {
  #' @param omega vetor de contantes
  #' @param f string: valor contendo qual parametro varia no tempo. Pode ser "mu", "sigma" ou "ambos".
  #' @param A lista de matrizes
  #' @param B lista de matrizes 
  #' @param n integer: Tamanho da serie simulada
  
  
  if (!f %in% c("sigma2", "mu", "ambos")){
    stop('o parametro f deve ser um entre "sigma", "mu" ou "ambos"')
  }
  
  order_max <- max(length(A), length(B))
  
  ## Modelando a média
  if(f == "mu"){ # Considero sigma2 = 1
    epsilon <- rnorm(n + order_max)
    mu <- rep(0, n)
    s <- rep(0, n)
    y <- rep(0, n)
    
    for(t in (order_max + 1):(n + order_max)){
      sum_A <- 0
      sum_B <- 0
      s[t-1] <- y[t-1] - mu[t-1]
      for(p in 1:length(A)){sum_A <- sum_A + A[p]*s[t - p]}
      #message(paste("soma A:", sum_A))
      for(q in 1:length(B)){sum_B <- sum_B + B[q]*mu[t - q]}
      #message(paste("soma B:", sum_B))
      mu[t] <- omega + sum_A + sum_B
      y[t] <- mu[t] + epsilon[t]
    }
    
    return(list(retornos = y, media = mu))
  }
  
  ## Modelando a variância
  if (f == "sigma2"){
    sigma2 <- rep(.5, n + order_max)
    s <- rep(0, n + order_max)
    y <- rep(1, n + order_max)
    epsilon <- rnorm(n + order_max)
    
    if (S == "Identity") {
      for (t in (order_max + 1):(n + order_max)) {
        sum_A <- 0
        sum_B <- 0
        s[t-1] <- (y[t-1]^2/(2*sigma2[t-1]^2)) - (1/(2*sigma2[t-1]))
        message(s[t-1])
        for(p in 1:length(A)){sum_A <- sum_A + A[p]*s[t - p]}
        message(paste("soma A:", sum_A))
        for(q in 1:length(B)){sum_B <- sum_B + B[q]*sigma2[t - q]}
        message(paste("soma B:", sum_B))
        sigma2[t] <- omega + sum_A + sum_B
        y[t] <- sqrt(sigma2[t])*epsilon[t]
      }
    }
    
    if (S == "Inv") {
      for (t in (order_max + 1):(n + order_max)) {
        sum_A <- 0
        sum_B <- 0
        s[t] <- (2*y[t]^2) - sigma2[t]
        for(p in 1:length(A)){sum_A <- sum_A + A[p]*s[t - p]}
        for(q in 1:length(B)){sum_B <- sum_B + B[p]*sigma2[t - p]}
        sigma2[t] <- omega + sum_A + sum_B
        y[t] <- sigma2[t]*epsilon[t]
      }
    }
    
    if (S == "SqrtInv") {
      for (t in (order_max + 1):(n + order_max)) {
        sum_A <- 0
        sum_B <- 0
        s[t] <- (sqrt(2)*y[t]^2/(sigma2[t])) - (sqrt(2)/2)
        for(p in 1:length(A)){sum_A <- sum_A + A[p]*s[t - p]}
        for(q in 1:length(B)){sum_B <- sum_B + B[p]*sigma2[t - p]}
        sigma2[t] <- omega + sum_A + sum_B
        y[t] <- sigma2[t]*epsilon[t]
      }
    }
    return(list(retornos=y, volatilidade=sigma2))
  }
  
  if (f == "ambos"){
    s <- rep(0, n)
    epsilon <- rnorm(n + order_max)
    y[t] <- rep(0, n)
    mu <- sigma2 <- rep(1, n)
    f <- list(mu=mu, sigma2=sigma2)
    
    if (S == "Identity") {
      for(t in (order_max + 1):(n + order_max)){
        # Matriz informação e gradiente
        mat_inf <- diag(2)
        grad <- c((y[t]/f$sigma2[t]) - f$mu[t],
                  ((y[t] - f$mu[t])^2/2*f$sigma2[t]^2) - (1/(2*f$sigma2[t])))
        s[t] <- mat_inf %*% grad
        
        sum_A <- 0
        sum_B <- 0
        for(p in 1:length(A)){sum_A <- sum_A + A[[p]]%*%s[t - p]}
        for(q in 1:length(B)){sum_B <- sum_B + B[[p]]%*%f$sigma2[t - p]}
        f$mu[t] <- omega[1] + sum_A[1] + sum_B[1]
        f$sigma2[t] <- omega[2] + sum_A[2] + sum_B[2]
        y[t] <- f$mu[t] + f$sigma2[t]*epsilon[t]
      }
    }
    
    if (S == "Inv") {
      for(t in 1:n){
        # Matriz informação e gradiente
        mat_inf <- matrix(c((f$mu[t]^2/f$sigma2[t]^2) + ((1 - 2*f$mu[t]^2)/f$sigma2[t]) + f$mu[t]^2, 
                            0, 0, 1), 
                          nrow = 2, byrow = T)
        grad <- c((y[t]/f$sigma2[t]) - f$mu[t],
                  ((y[t] - f$mu[t])^2/2*f$sigma2[t]^2) - (1/(2*f$sigma2[t])))
        s[t] <- solve(mat_inf) %*% grad
        
        sum_A <- 0
        sum_B <- 0
        for(p in 1:length(A)){sum_A <- sum_A + A[[p]]%*%s[t - p]}
        for(q in 1:length(B)){sum_B <- sum_B + B[[p]]%*%f$sigma2[t - p]}
        f$mu[t] <- omega[1] + sum_A[1] + sum_B[1]
        f$sigma2[t] <- omega[2] + sum_A[2] + sum_B[2]
        y[t] <- f$mu[t] + f$sigma2[t]*epsilon[t]
      }
    }
    
    if (S == "SqtrInv") {
      for(t in 1:n){
        # Matriz informação e gradiente
        mat_inf <- matrix(c((f$mu[t]^2/f$sigma2[t]^2) + ((1 - 2*f$mu[t]^2)/f$sigma2[t]) + f$mu[t]^2, 
                            0, 0, 1), 
                          nrow = 2, byrow = T)
        grad <- c((y[t]/f$sigma2[t]) - f$mu[t],
                  ((y[t] - f$mu[t])^2/2*f$sigma2[t]^2) - (1/(2*f$sigma2[t])))
        s[t] <- sqrt(solve(mat_inf)) %*% grad
        
        sum_A <- 0
        sum_B <- 0
        for(p in 1:length(A)){sum_A <- sum_A + A[[p]]%*%s[t - p]}
        for(q in 1:length(B)){sum_B <- sum_B + B[[p]]%*%f$sigma2[t - p]}
        f$mu[t] <- omega[1] + sum_A[1] + sum_B[1]
        f$sigma2[t] <- omega[2] + sum_A[2] + sum_B[2]
        y[t] <- f$mu[t] + f$sigma2[t]*epsilon[t]
      }
    }
  }
  return(list(y, f))
}

################################################################################
##############                      Testando                  ##################
################################################################################

## Media

## Volatilidade
omega <- .2; f <- "sigma2"; A <- .3; B <- .2; S <- "Identity"; n <- 10
serie <- gas_sim(omega, f, A, B, S, n)

ts.plot(serie$retornos)
ts.plot(serie$volatilidade)

## ambos






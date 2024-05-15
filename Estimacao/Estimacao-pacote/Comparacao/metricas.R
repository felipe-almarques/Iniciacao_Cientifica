metricas <- function(erros){
  n <- nrow(erros[[1]])
  
  RMSE <- list()
  rmse <- list()
  for (lista in 1:3) {
    aux <- 1
    for(i in c(1, 5, 9)) {
      rmse[[aux]] <- c(garch=sum((erros[[lista]][i] - erros[[lista]][i+1])^2) / n,
                    sv=sum((erros[[lista]][i] - erros[[lista]][i+2])^2) / n,
                    gas=sum((erros[[lista]][i] - erros[[lista]][i+3])^2) / n)
      aux <- aux + 1
    }
    names(rmse) <- c("amostra.garch", "amostra.sv", "amostra.gas")
    RMSE[[lista]] <- rmse
  }
  names(RMSE) <- c("n_500", "n_1000", "n_2500")
  return(RMSE)
}

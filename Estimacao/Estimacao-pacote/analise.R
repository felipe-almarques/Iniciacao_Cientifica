###############################################################################
##############                Comparacao entre os Modelos        ##############
###############################################################################
library(tidyverse)
source("Comparacao/metricas.R")

# função
rmse <- function(verdadeiro, atual, n=10000){
  return(sqrt(sum((verdadeiro - atual)^2) / n))
}

comp <- list()

## Amostra de tamanho n=500
dados500 <- read_csv("artifacts/erros500.csv")

# amostra garch
RMSE.GARCH_garch <- rmse(dados500$volgarch_verdadeira, dados500$volgarch_garch)
RMSE.GARCH_sv <- rmse(dados500$volgarch_verdadeira, dados500$volgarch_sv)
RMSE.GARCH_gas <- rmse(dados500$volgarch_verdadeira, dados500$volgarch_gas)

# amostra sv
RMSE.SV_garch <- rmse(dados500$volsv_verdadeira, dados500$volsv_garch)
RMSE.SV_sv <- rmse(dados500$volsv_verdadeira, dados500$volsv_sv)
RMSE.SV_gas <- rmse(dados500$volsv_verdadeira, dados500$volsv_gas)

# amostra GAS
RMSE.GAS_garch <- rmse(dados500$volgas_verdadeira, dados500$volgas_garch)
RMSE.GAS_sv <- rmse(dados500$volgas_verdadeira, dados500$volgas_sv)
RMSE.GAS_gas <- rmse(dados500$volgas_verdadeira, dados500$volgas_gas)

comp[["500"]] <- list(garch=c(RMSE.GARCH_garch, RMSE.GARCH_sv, RMSE.GARCH_gas),
     sv=c(RMSE.SV_garch, RMSE.SV_sv, RMSE.SV_gas),
     gas=c(RMSE.GAS_garch, RMSE.GAS_sv, RMSE.GAS_gas))

# Para a amostra de 500, o modelo gas foi superior em todos.

## Amostra de tamanho n=1000
dados1000 <- read_csv("artifacts/erros1000.csv")

# amostra garch
RMSE.GARCH_garch2 <- rmse(dados1000$volgarch_verdadeira, dados1000$volgarch_garch)
RMSE.GARCH_sv2 <- rmse(dados1000$volgarch_verdadeira, dados1000$volgarch_sv)
RMSE.GARCH_gas2 <- rmse(dados1000$volgarch_verdadeira, dados1000$volgarch_gas)

# amostra sv
RMSE.SV_garch2 <- rmse(dados1000$volsv_verdadeira, dados1000$volsv_garch)
RMSE.SV_sv2 <- rmse(dados1000$volsv_verdadeira, dados1000$volsv_sv)
RMSE.SV_gas2 <- rmse(dados1000$volsv_verdadeira, dados1000$volsv_gas)

# amostra GAS
RMSE.GAS_garch2 <- rmse(dados1000$volgas_verdadeira, dados1000$volgas_garch)
RMSE.GAS_sv2 <- rmse(dados1000$volgas_verdadeira, dados1000$volgas_sv)
RMSE.GAS_gas2 <- rmse(dados1000$volgas_verdadeira, dados1000$volgas_gas)

comp[["1000"]] <- list(
  garch=c(RMSE.GARCH_garch2, RMSE.GARCH_sv2, RMSE.GARCH_gas2),
  sv=c(RMSE.SV_garch2, RMSE.SV_sv2, RMSE.SV_gas2),
  gas=c(RMSE.GAS_garch2, RMSE.GAS_sv2, RMSE.GAS_gas2)
  )

## Amostra de tamanho n=1000
dados2500 <- read_csv("artifacts/erros2500.csv")

# amostra garch
RMSE.GARCH_garch3 <- rmse(dados2500$volgarch_verdadeira, dados2500$volgarch_garch)
RMSE.GARCH_sv3 <- rmse(dados2500$volgarch_verdadeira, dados2500$volgarch_sv)
RMSE.GARCH_gas3 <- rmse(dados2500$volgarch_verdadeira, dados2500$volgarch_gas)

# amostra sv
RMSE.SV_garch3 <- rmse(dados2500$volsv_verdadeira, dados2500$volsv_garch)
RMSE.SV_sv3 <- rmse(dados2500$volsv_verdadeira, dados2500$volsv_sv)
RMSE.SV_gas3 <- rmse(dados2500$volsv_verdadeira, dados2500$volsv_gas)

# amostra GAS
RMSE.GAS_garch3 <- rmse(dados2500$volgas_verdadeira, dados2500$volgas_garch)
RMSE.GAS_sv3 <- rmse(dados2500$volgas_verdadeira, dados2500$volgas_sv)
RMSE.GAS_gas3 <- rmse(dados2500$volgas_verdadeira, dados2500$volgas_gas)

comp[["2500"]] <- list(
  garch=c(RMSE.GARCH_garch3, RMSE.GARCH_sv3, RMSE.GARCH_gas3),
  sv=c(RMSE.SV_garch3, RMSE.SV_sv3, RMSE.SV_gas3),
  gas=c(RMSE.GAS_garch3, RMSE.GAS_sv3, RMSE.GAS_gas3)
)

comp

# Para a amostra de 1000, o modelo gas foi superior em todos.


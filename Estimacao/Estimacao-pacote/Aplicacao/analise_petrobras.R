#############################################################################
###############         Analise de dados da Petrobras         ###############
#############################################################################

## Pacotes
library(tidyverse)
library(stochvol)
library(rugarch)
library(GAS)
library(TSA)

## funcoes
diagnostico <- function(modelo){
  e_hat <- modelo@fit$residuals/modelo@fit$sigma
  
  op = par(mfrow=c(1,3))
  acf(e_hat)
  acf(e_hat^2)
  plot(modelo, which = 9)
  par(op)
}



## dados
dados <- read_csv("artifacts/PETR4_data.csv")

glimpse(dados)

## Análise Exploratoria
ts.plot(dados$ret)
acf(dados$ret)
acf(dados$ret^2)

#### Modelagem de dados
# Garch - normal
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), 
                                     include.mean = FALSE))
modelo_garch <- ugarchfit(spec, dados$ret)

modelo_garch
diagnostico(modelo_garch)
round(modelo_garch@fit$coef, 2)

# omega alpha1  beta1 
# 0.00   0.11   0.87 

## Ar-Garch - normal
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 0), 
                                     include.mean = FALSE))
modelo_argarch_norm <- ugarchfit(spec, dados$ret)

modelo_argarch_norm
diagnostico(modelo_argarch_norm)

round(modelo_argarch_norm@fit$coef, 2)

## Garch - t
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), 
                                     include.mean = FALSE),
                   distribution.model = "std")
modelo_garch_t <- ugarchfit(spec, dados$ret)

modelo_garch_t
diagnostico(modelo_garch_t)
round(modelo_garch_t@fit$coef, 2)

## Ar-Garch - t
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 0), 
                                     include.mean = FALSE),
                   distribution.model = "std")
modelo_argarch_t <- ugarchfit(spec, dados$ret)

modelo_argarch_t
diagnostico(modelo_argarch_t)

round(modelo_argarch_t@fit$coef, 2)

#   mu       phi      sigma    exp(mu/2)  sigma^2 
# -7.61     0.96      0.21      0.02       0.04 

# Stochastic Volatility
modelo_sv <- svsample(dados$ret)
round(modelo_sv$summary$para[,1], 2)

# GAS 
spec <- UniGASSpec()
modelo_gas <- UniGASFit(spec, data = dados$ret)
round(modelo_gas@Estimates$Inference, 2)

# kappa1    kappa2    a2    b2
#   0       -0.26    0.19  0.96

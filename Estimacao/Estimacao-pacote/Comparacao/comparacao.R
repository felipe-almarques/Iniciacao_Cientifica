###########################################################################################              Comparação de Modelos              ################
##############################################################################

## Pacotes
library(tidyverse)
library(TSA)
library(rugarch)
library(stochvol)
library(GAS)
library(gmailr)
library(glue)
library(progress)

source("Comparacao/estimacao.R")
source("Comparacao/metricas.R")
source("Comparacao/email.R")
message <- base::message

#### Simulação ####
## Setando os tamanhos
n <- c(500, 1000, 2500)
M <- 1000

## Definindo variáveis e parâmetros
erros <- list()
garch_param <- list(alpha = c(0.00002 , 0.11), beta = 0.87)
sv_param <- list(mu = -7.61, phi = 0.96, sigma = 0.21)
gas_param <- list(kappa = c(0, -0.26), 
                  A = matrix(c(0, 0, 0, 0.19), ncol = 2), 
                  B = matrix(c(0, 0, 0, 0.96), ncol = 2), 
                  dist = "norm", scaling = "Identity")

## Iniciando o loop
for (i in 1:length(n)) {
  message(paste0("Iteração ", i, ": n = ", n[i]))
  erros[[i]] <- estimacao(n[i], M, garch_param, sv_param, gas_param)
}
message("Finalizado!")
#### Calculando as métricas ####
rmse <- metricas(erros)

#### Enviando o email ####

enviar_email(erros, rmse)

# ugarchfilter-->error: parameters names do not match specification
# Expected Parameters are: omega alpha1 beta1



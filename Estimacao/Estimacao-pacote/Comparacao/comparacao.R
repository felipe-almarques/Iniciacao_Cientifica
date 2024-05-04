#########################################################################################################################
###################                           Compara??o de Modelos                                   ###################
#########################################################################################################################

## Pacotes
library(tidyverse)
library(TSA)
library(rugarch)
library(stochvol)
library(GAS)

source("Comparacao/estimacao.R")

## Setando os tamanhos
n <- c(500, 1000, 2500)
M <- 100

## Definindo variáveis e parâmetros
erros <- list()
garch_param <- list(alpha = c(.1, .2), beta = .2)
sv_param <- list(mu = -10, phi = .99, sigma = .2)
gas_param <- list(kappa = c(0, .1), A = matrix(c(0, 0, 0, .3), ncol = 2), 
B = matrix(c(0, 0, 0, .2), ncol = 2), dist = "norm", scaling = "Identity")


## Iniciando o loop
for (i in 1:length(n)) {
  message(paste0("Iteração ", i, ": n = ", n[i]))
  erros[[i]] <- estimacao(n[i], M, garch_param, sv_param, gas_param)
}

view(erros[[1]])

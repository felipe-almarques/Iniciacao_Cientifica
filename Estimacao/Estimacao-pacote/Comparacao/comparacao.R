#########################################################################################################################
###################                           Comparação de Modelos                                   ###################
#########################################################################################################################

## Pacotes
library(TSA)
library(rugarch)
library(stochvol)
library(GAS)

source("estimacao")

## Setando os tamanhos
n <- c(500, 1000, 2500)
M <- 2

## Definindo variáveis
erros <- list()

## Iniciando o loop
for (i in 1:length(n)) {
  message(paste0("Iteração ", i, " de ", length(n)))
  erros[[i]] <- estimacao(n[i], M)
}

###########################################################################################              Comparação Extensa de Modelos           #############
###############################################################################

## Pacotes
library(tidyverse)
library(TSA)
library(rugarch)
library(stochvol)
library(GAS)
library(gmailr)
library(glue)
library(progress)
message <- base::message


#### Funções
estimacao <- function(n, M, garch_param, sv_param, gas_param) {
  # garch_param: list(alpha, beta)
  # sv_param: list(mu, phi, sigma)
  # gas_param: list(kappa, A, B, dist, scaling)
  
  ## Definindo a progress bar
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = M,
                         complete = "=",   
                         incomplete = "-", 
                         current = ">",    
                         clear = FALSE,
                         width = 100)      
  
  ## Definindo os vetores de previsões
  volgarch_verdadeira <- rep(0, M) ; volgarch_garch <- rep(0, M)
  volgarch_sv <- rep(0, M) ; volgarch_gas <- rep(0, M)
  volsv_verdadeira <- rep(0, M) ; volsv_garch <- rep(0, M)
  volsv_sv <- rep(0, M) ; volsv_gas <- rep(0, M)
  volgas_verdadeira <- rep(0, M) ; volgas_garch <- rep(0, M)
  volgas_sv <- rep(0, M) ; volgas_gas <- rep(0, M)
  
  ## Definindo parametros
  alpha <- garch_param$alpha ; beta <- garch_param$beta
  mu <- sv_param$mu ; phi <- sv_param$phi ; sigma <- sv_param$sigma
  kappa <- gas_param$kappa ; A <- gas_param$A
  B <- gas_param$B ; dist <- gas_param$dist ; Scaling <- gas_param$scaling
  
  ## Definindo modelos
  spec_garch <- ugarchspec(mean.model = list(armaOrder = c(0, 0), 
                                             include.mean = F))
  spec_gas <- UniGASSpec(Dist = dist, ScalingType = Scaling, 
                         GASPar = list(scale = TRUE))
  
  for (t in 1:M) {
    if((t/M) %in% seq(0,1,.25)){
      email_aviso(i=t)
    } 
    flag <- TRUE
    while (flag) {
      tryCatch(
        expr = {
          ## Simulacao
          amostra_garch <- garch.sim(alpha, beta, n + 1)
          amostra_sv <- svsim(n + 1, mu, phi, sigma)
          amostra_gas <- UniGASSim(T.sim = n + 1, kappa = kappa, 
                                   A = A, B = B, Dist = dist, 
                                   ScalingType = Scaling)
          
          ## Estimacao
          # Garch
          fit_garch1 <- ugarchfit(spec_garch, amostra_garch[1:n])
          fit_sv1 <- svsample(amostra_garch[1:n], quiet = TRUE)
          fit_gas1 <- UniGASFit(spec_gas, amostra_garch[1:n])
          # SV
          fit_garch2 <- ugarchfit(spec_garch, amostra_sv$y[1:n])
          fit_sv2 <- svsample(amostra_sv$y[1:n], quiet = TRUE)
          fit_gas2 <- UniGASFit(spec_gas, amostra_sv$y[1:n])
          # Gas
          fit_garch3 <- ugarchfit(spec_garch, amostra_gas@Data$vY[1:n])
          fit_sv3 <- svsample(amostra_gas@Data$vY[1:n], quiet = TRUE)
          fit_gas3 <- UniGASFit(spec_gas, amostra_gas@Data$vY[1:n])
          
          ## Previsao One-step-ahead
          # Amostra Garch
          fore_garch1 <- ugarchforecast(fit_garch1, n.ahead = 1)
          fore_sv1 <- predict(fit_sv1, 1)
          fore_gas1 <- UniGASFor(fit_gas1, H = 1)
          
          volgarch_verdadeira[t] <- amostra_garch[n+1]
          volgarch_garch[t] <- fore_garch1@forecast$sigmaFor
          volgarch_sv[t] <- summary(fore_sv1$vol)$statistics[1]
          volgarch_gas[t] <- fore_gas1@Forecast$PointForecast[2]
          
          # Amostra SV
          fore_garch2 <- ugarchforecast(fit_garch2, n.ahead = 1)
          fore_sv2 <- predict(fit_sv2, 1)
          fore_gas2 <- UniGASFor(fit_gas2, H = 1)
          
          volsv_verdadeira[t] <- amostra_sv$y[n+1]
          volsv_garch[t] <- fore_garch2@forecast$sigmaFor
          volsv_sv[t] <- summary(fore_sv2$vol)$statistics[1]
          volsv_gas[t] <- fore_gas2@Forecast$PointForecast[2]
          
          # Amostra GAS
          fore_garch3 <- ugarchforecast(fit_garch3, n.ahead = 1)
          fore_sv3 <- predict(fit_sv3, 1)
          fore_gas3 <- UniGASFor(fit_gas3, H = 1)
          
          volgas_verdadeira[t] <- amostra_gas@Data$vY[n+1]
          volgas_garch[t] <- fore_garch3@forecast$sigmaFor
          volgas_sv[t] <- summary(fore_sv3$vol)$statistics[1]
          volgas_gas[t] <- fore_gas3@Forecast$PointForecast[2]
          
          pb$tick()
          flag <- FALSE
        },
        error = function(cond) {
          message("Erro na iteracao")
          message("Mensagem de erro:")
          message(conditionMessage(cond))
          message("Simulando novamente os dados")
        }
      )
    }
  }
  
  dados <- data.frame(volgarch_verdadeira, volgarch_garch, volgarch_sv,
                      volgarch_gas, volsv_verdadeira, volsv_garch, 
                      volsv_sv, volsv_gas, volgas_verdadeira, volgas_garch,
                      volgas_sv, volgas_gas)
  return(dados)
}

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

enviar_email <- function(erros, RMSE){
  
  
  ## conectando com o servidor
  path <- "chave_acesso_email_r.json"
  gm_auth_configure(client = gargle::gargle_oauth_client_from_json(path))
  
  ## construindo o email
  texto <- glue("A simulacao dos modelos ja terminou!\nA metrica calculada para essa simulacao foi: RMSE.\nOs resultados sao apresentados abaixo:\n\nPara n = 500:\n  - Amostra Garch:\n    garch = {round(RMSE$n_500$amostra.garch[1],4)}; sv = {round(RMSE$n_500$amostra.garch[2],4)}; gas = {round(RMSE$n_500$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_500$amostra.sv[1],4)}; sv = {round(RMSE$n_500$amostra.sv[2],4)}; gas = {round(RMSE$n_500$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_500$amostra.gas[1],4)}; sv = {round(RMSE$n_500$amostra.gas[2],4)}; gas = {round(RMSE$n_500$amostra.gas[3],4)}\n\nPara n = 1000:\n  - Amostra Garch:\n    garch = {round(RMSE$n_1000$amostra.garch[1],4)}; sv = {round(RMSE$n_1000$amostra.garch[2],4)}; gas = {round(RMSE$n_1000$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_1000$amostra.sv[1],4)}; sv = {round(RMSE$n_1000$amostra.sv[2],4)}; gas = {round(RMSE$n_1000$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_1000$amostra.gas[1],4)}; sv = {round(RMSE$n_1000$amostra.gas[2],4)}; gas = {round(RMSE$n_1000$amostra.gas[3],4)}\n\nPara n = 2500:\n  - Amostra Garch:\n    garch = {round(RMSE$n_2500$amostra.garch[1],4)}; sv = {round(RMSE$n_2500$amostra.garch[2],4)}; gas = {round(RMSE$n_2500$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_2500$amostra.sv[1],4)}; sv = {round(RMSE$n_2500$amostra.sv[2],4)}; gas = {round(RMSE$n_2500$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_2500$amostra.gas[1],4)}; sv = {round(RMSE$n_2500$amostra.gas[2],4)}; gas = {round(RMSE$n_2500$amostra.gas[3],4)}\n\nOs arquivos contendo os resultados simulados estao anexados neste email.\n\nAtenciosamente,\nFelipe.")
  
  
  email <- gm_mime() %>%
    gm_to(c("felipealbuquerquemarques@gmail.com", 
            "f236106@dac.unicamp.br")) %>% 
    gm_from("f236106@dac.unicamp.br") %>% 
    gm_subject("[IC] Resultado Simulação") %>% 
    gm_text_body(texto)
  
  ## anexando os arquivos
  write.csv(erros[[1]], file="artifacts/erros_500.csv")
  write.csv(erros[[2]], file="artifacts/erros_1000.csv")
  write.csv(erros[[3]], file="artifacts/erros_2500.csv")
  save(erros, file="artifacts/erros.RData")
  
  email <- gm_attach_file(email, "artifacts/erros.RData")
  email <- gm_attach_file(email, "artifacts/erros_500.csv")
  email <- gm_attach_file(email, "artifacts/erros_1000.csv")
  email <- gm_attach_file(email, "artifacts/erros_2500.csv")
  
  #gm_create_draft(email)
  
  ## enviando o email
  gm_auth(email = "f236106@dac.unicamp.br")
  
  gm_send_message(email)
  
}

email_aviso <- function(i=1, metricas=FALSE, j=1, erros) {
  
  ## conectando com o servidor
  path <- "chave_acesso_email_r.json"
  gm_auth_configure(client = gargle::gargle_oauth_client_from_json(path))
  
  
  ## Construindo o email interno (progresso para o mesmo tamanho amostral)
  if (metricas == FALSE) {
    texto <- glue("A simulacao continua rodando a todo vapor! Ja estamos na iteracao {i}.\n Em breve retornaremos com mais atualizacoes.")
    
    
    email <- gm_mime() %>%
      gm_to(c("felipealbuquerquemarques@gmail.com", 
              "f236106@dac.unicamp.br")) %>% 
      gm_from("f236106@dac.unicamp.br") %>% 
      gm_subject("[IC] Progresso da Simulacao") %>% 
      gm_text_body(texto)
  }
  
  ## construindo o email externo (diferentes tamanhos amostrais)
  if (metricas) {
    texto <- glue("A simulacao concluiu mais um passo! Ja finalizamos a amostra {j}.\nOs resultados simulados se encontram em anexo.\n\nAtenciosamente,\nFelipe")
    
    
    email <- gm_mime() %>%
      gm_to(c("felipealbuquerquemarques@gmail.com", 
              "f236106@dac.unicamp.br")) %>% 
      gm_from("f236106@dac.unicamp.br") %>% 
      gm_subject("[IC] Progresso da Simulacao") %>% 
      gm_text_body(texto)
    
    ## anexando os arquivos
    write.csv(erros, file="artifacts/erros_temporario.csv")
    save(erros, file="artifacts/erros_temporario.RData")
    
    email <- gm_attach_file(email, "artifacts/erros_temporario.RData")
    email <- gm_attach_file(email, "artifacts/erros_temporario.csv")
  }
  
  ## enviando o email
  gm_auth(email = "f236106@dac.unicamp.br")
  
  gm_send_message(email)
}

#### Simulação ####
## Setando os tamanhos
n <- c(500, 1000, 2500)
M <- 100

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
  email_aviso(metricas=TRUE, j=i, erros=erros[[i]])
}
message("Finalizado!")

#### Calculando as métricas ####
rmse <- metricas(erros)

#### Enviando o email ####
enviar_email(erros, rmse)











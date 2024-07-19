enviar_email <- function(erros, RMSE){

  
  ## conectando com o servidor
  path <- "chave_acesso_email_r.json"
  gm_auth_configure(client = gargle::gargle_oauth_client_from_json(path))
  
  ## construindo o email
  texto <- glue("A simulacao dos modelos ja terminou!\nA metrica calculada para essa simulacao foi: RMSE.\nOs resultados sao apresentados abaixo:\n\nPara n = 500:\n  - Amostra Garch:\n    garch = {round(RMSE$n_500$amostra.garch[1],4)}; sv = {round(RMSE$n_500$amostra.garch[2],4)}; gas = {round(RMSE$n_500$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_500$amostra.sv[1],4)}; sv = {round(RMSE$n_500$amostra.sv[2],4)}; gas = {round(RMSE$n_500$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_500$amostra.gas[1],4)}; sv = {round(RMSE$n_500$amostra.gas[2],4)}; gas = {round(RMSE$n_500$amostra.gas[3],4)}\n\nPara n = 1000:\n  - Amostra Garch:\n    garch = {round(RMSE$n_1000$amostra.garch[1],4)}; sv = {round(RMSE$n_1000$amostra.garch[2],4)}; gas = {round(RMSE$n_1000$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_1000$amostra.sv[1],4)}; sv = {round(RMSE$n_1000$amostra.sv[2],4)}; gas = {round(RMSE$n_1000$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_1000$amostra.gas[1],4)}; sv = {round(RMSE$n_1000$amostra.gas[2],4)}; gas = {round(RMSE$n_1000$amostra.gas[3],4)}\n\nPara n = 2500:\n  - Amostra Garch:\n    garch = {round(RMSE$n_2500$amostra.garch[1],4)}; sv = {round(RMSE$n_2500$amostra.garch[2],4)}; gas = {round(RMSE$n_2500$amostra.garch[3],4)}\n  - Amostra SV:\n    garch = {round(RMSE$n_2500$amostra.sv[1],4)}; sv = {round(RMSE$n_2500$amostra.sv[2],4)}; gas = {round(RMSE$n_2500$amostra.sv[3],4)}\n  - Amostra Gas:\n    garch = {round(RMSE$n_2500$amostra.gas[1],4)}; sv = {round(RMSE$n_2500$amostra.gas[2],4)}; gas = {round(RMSE$n_2500$amostra.gas[3],4)}\n\nOs arquivos contendo os resultados simulados estao anexados neste email.\n\nAtenciosamente,\nFelipe.")
  
  
  email <- gm_mime() %>%
    gm_to("felipealbuquerquemarques@gmail.com") %>% 
    gm_from("f236106@dac.unicamp.br") %>% 
    gm_subject("[IC] Resultado Simulacao") %>% 
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


email_aviso <- function(i=1, metricas=FALSE, j=1, erros, init=FALSE) {
  
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
  
  if (init) {
    texto <- glue("A simulacao acabou de comecar!\n Em breve retornaremos com mais atualizacoes.")
    
    
    email <- gm_mime() %>%
      gm_to(c("felipealbuquerquemarques@gmail.com", 
              "f236106@dac.unicamp.br")) %>% 
      gm_from("f236106@dac.unicamp.br") %>% 
      gm_subject("[IC] Progresso da Simulacao") %>% 
      gm_text_body(texto)
  }
  
  ## enviando o email
  gm_auth(email = "f236106@dac.unicamp.br")
  
  gm_send_message(email)
}

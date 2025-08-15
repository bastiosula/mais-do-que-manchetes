# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Avaliando as variáveis X contra Y com VAR, Granger, IRFs e FEDV

avaliar_variáveis_com_VAR <- function(ação, seleção, método, ajuste) {
  
  # Pacotes
  library(arrow)
  library(vars)
  library(sandwich)
  library(lmtest)
  library(readxl)
  
  # Configuração
  setwd("C:\\Users\\User\\OneDrive\\3. Estudos\\2. Malha fina do TCC para publicação\\Códigos\\3. Modelagem econométrica")
  caminho <- sprintf("resultados do LASSO/%s - total - %s - %s - %s - variáveis_selecionadas.xlsx",
                     seleção, método, ajuste, ação)
  dados <- read_excel(caminho)
  
  nomes_da_variável_Y <- list(
    "PETR3" = "Ação_da_Petrobras",
    "PETR4" = "Ação_da_Petrobras"
  )
  variável_Y <- nomes_da_variável_Y[[ação]]
  
  
  # AICs
  AICs_testados <- sapply(1:12, function(p) AIC(VAR(dados, p = p, type = "const")))
  coeficientes_de_informação <- data.frame(defasagem = 1:12, AIC = AICs_testados)
  defasagem_ótima <- which.min(AICs_testados)
  
  
  # VAR robusto à heterocedasticidade (com do.call)
  modelo_de_VAR <- do.call(VAR, list(y = dados, p = defasagem_ótima, type = "const"))
  covariância_robusta <- vcovHC(modelo_de_VAR, type = "HC1")
  resultados_do_VAR <- coeftest(modelo_de_VAR, vcov. = covariância_robusta)
  
    # Extração da equação associada à variável dependente
  resultados_do_VAR_contra_Y <- resultados_do_VAR[
    grep(variável_Y, rownames(resultados_do_VAR), value = TRUE),
    , drop = FALSE
  ]
  resultados_do_VAR_contra_Y <- as.data.frame(resultados_do_VAR_contra_Y)
  colnames(resultados_do_VAR_contra_Y) <- c("coeficiente", "desvio_padrão", "estatística_t", "p_valor")
  resultados_do_VAR_contra_Y <- resultados_do_VAR_contra_Y[resultados_do_VAR_contra_Y$p_valor <= 0.10, ]
  resultados_do_VAR_contra_Y$significância <- ifelse(
    resultados_do_VAR_contra_Y$p_valor < 0.01, 0.01,
    ifelse(resultados_do_VAR_contra_Y$p_valor < 0.05, 0.05, 0.10)
  )
  
  
  # Causalidade de Granger 
  nomes_das_variáveis <- names(dados)
  variáveis_explicativas <- setdiff(nomes_das_variáveis, variável_Y)
  
    # Inicializando o data.frame de resultados
  resultados_da_causalidade_de_Granger <- data.frame(
    causa = character(),
    efeito = character(),
    p_valor = numeric(),
    significância = numeric(),
    stringsAsFactors = FALSE
  )
  
    # Testes: X → Y
  for (x in variáveis_explicativas) {
    teste <- causality(modelo_de_VAR, cause = x)
    p <- teste$Granger$p.value
    sig <- ifelse(p < 0.01, 0.01, ifelse(p <= 0.05, 0.05, ifelse(p <= 0.10, 0.10, 1)))
    resultados_da_causalidade_de_Granger <- rbind(
      resultados_da_causalidade_de_Granger,
      data.frame(causa = x, efeito = variável_Y, p_valor = p, significância = sig)
    )
  }
  
    # Testes: Y → X
  for (x in variáveis_explicativas) {
    teste <- causality(modelo_de_VAR, cause = variável_Y)
    p <- teste$Granger$p.value
    sig <- ifelse(p < 0.01, 0.01, ifelse(p <= 0.05, 0.05, ifelse(p <= 0.10, 0.10, 1)))
    resultados_da_causalidade_de_Granger <- rbind(
      resultados_da_causalidade_de_Granger,
      data.frame(causa = variável_Y, efeito = x, p_valor = p, significância = sig)
    )
  }
  
    # Filtrando apenas os significativos
  resultados_da_causalidade_de_Granger <- subset(resultados_da_causalidade_de_Granger, p_valor <= 0.10)
  

  # IRFs apenas das variáveis que causam variável_Y
  variáveis_significativas <- variáveis_explicativas#resultados_da_causalidade_de_Granger$causa[resultados_da_causalidade_de_Granger$efeito == variável_Y]
  
    # Lista temporária para armazenar os dataframes das IRFs
  lista_de_IRFs <- list()
  
  for (variável in variáveis_significativas) {
    resposta_impulso <- irf(modelo_de_VAR, impulse = variável, response = variável_Y,
                            n.ahead = 12, boot = TRUE)
    defasagem <- 0:12
    impulso_resposta <- as.numeric(resposta_impulso$irf[[1]][, variável_Y])
    banda_inferior <- as.numeric(resposta_impulso$Lower[[1]][, variável_Y])
    banda_superior <- as.numeric(resposta_impulso$Upper[[1]][, variável_Y])
    
    IRF_individual <- data.frame(
      variável = variável,
      defasagem = defasagem,
      impulso_resposta = impulso_resposta,
      banda_inferior = banda_inferior,
      banda_superior = banda_superior
    )
    
    lista_de_IRFs[[variável]] <- IRF_individual
  }
  
    # Empilhar os dataframes em um único
  funções_impulso_resposta_contra_Y <- do.call(rbind, lista_de_IRFs)
  rownames(funções_impulso_resposta_contra_Y) <- NULL
  
  
  # FEVD
  decomposição_da_variância <- fevd(modelo_de_VAR, n.ahead = 12)
  FEVD_da_variável_Y <- data.frame(
    defasagem = 1:nrow(decomposição_da_variância[[variável_Y]]),
    decomposição_da_variância[[variável_Y]]
  )
  
  return(list(
    coeficientes_de_informação = coeficientes_de_informação,
    resultados_do_VAR_contra_Y = resultados_do_VAR_contra_Y,
    resultados_da_causalidade_de_Granger = resultados_da_causalidade_de_Granger,
    funções_impulso_resposta_contra_Y = funções_impulso_resposta_contra_Y,
    FEVD_da_variável_Y = FEVD_da_variável_Y
  ))
}

# # # # # # # # # # # # # # # # # # # # # # # # 
# Exportando os resultados em um arquivo agregado

exportar_resultados <- function(ação, seleção, método, ajuste, resultados) {
  
  # Pacotes
  library(openxlsx)
  
  # Caminho do arquivo
  setwd("C:\\Users\\User\\OneDrive\\3. Estudos\\2. Malha fina do TCC para publicação\\Códigos\\3. Modelagem econométrica")
  arquivo <- sprintf("resultados do VAR/%s - total - %s - %s - %s.xlsx",
                     seleção, método, ajuste, ação)
  
  
  # Criação do workbook
  wb <- createWorkbook()
  
  addWorksheet(wb, "AIC")
  writeData(wb, "AIC", resultados$coeficientes_de_informação)
  
  addWorksheet(wb, "VAR")
  writeData(wb, "VAR", cbind(variáveis = rownames(resultados$resultados_do_VAR_contra_Y),
                             resultados$resultados_do_VAR_contra_Y))
  
  addWorksheet(wb, "Granger")
  writeData(wb, "Granger", resultados$resultados_da_causalidade_de_Granger)
  
  addWorksheet(wb, "IRFs")
  writeData(wb, "IRFs", resultados$funções_impulso_resposta_contra_Y)
  
  addWorksheet(wb, "FEVD")
  writeData(wb, "FEVD", resultados$FEVD_da_variável_Y)
  
  
  # Salvando o arquivo
  saveWorkbook(wb, arquivo, overwrite = TRUE)
}

# # # # # # # # # # #
# Modelagem individual

{
ação <- "PETR4"
seleção <- "valor"
método <- "online"
ajuste <- "ajustado"

library(readxl)
setwd("C:\\Users\\User\\OneDrive\\3. Estudos\\2. Malha fina do TCC para publicação\\Códigos\\3. Modelagem econométrica")
caminho <- sprintf("resultados do LASSO/%s - total - %s - %s - %s - variáveis_selecionadas.xlsx",
                   seleção, método, ajuste, ação)
dados <- read_excel(caminho)

resultados <- avaliar_variáveis_com_VAR(ação, seleção, método, ajuste)
exportar_resultados(ação, seleção, método, ajuste, resultados)
}

# # # # # # # # # # # 
# Modelagem massificada

{
library(iterators)
library(parallel)
library(foreach)
library(doParallel)

# Criando todas as combinações
combinações <- expand.grid(
  ação = c("PETR4", "PETR3"),
  seleção = c("valor", "globorural", "epocanegocios", "valorinveste", "revistapegn"),
  método = c("batch", "online"),
  ajuste = c("padrão", "ajustado"),
  stringsAsFactors = FALSE
)

# Configurando cluster para paralelização
n_cores <- detectCores() - 1  
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Executando em paralelo
resultados <- foreach(
  i = 1:nrow(combinações), 
  .packages = c(),  # adicione aqui os pacotes que suas funções precisam, ex: c("dplyr", "forecast")
  .errorhandling = "pass",
  .combine = "c"  # combina os resultados em um vetor
) %dopar% {
  
  # Pegar a combinação atual
  comb <- combinações[i, ]
  
  # Log do que está sendo processado
  cat(sprintf("Processando: %s - %s - %s - %s (Worker %d)\n", 
              comb$ação, comb$seleção, comb$método, comb$ajuste, 
              Sys.getpid()))
  
  # Tentar executar
  tryCatch({
    resultado_modelo <- avaliar_variáveis_com_VAR(comb$ação, comb$seleção, comb$método, comb$ajuste)
    exportar_resultados(comb$ação, comb$seleção, comb$método, comb$ajuste, resultado_modelo)
  }, error = function(e) {
    message(sprintf("Erro em: %s | %s | %s | %s", seleção, método, ajuste, ação))
  })
}

# Parar o cluster
stopCluster(cl)
}

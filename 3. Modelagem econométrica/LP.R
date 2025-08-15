
setwd("C:/Users/User/OneDrive/3. Estudos/2. Malha fina do TCC para publicação/Códigos/3. Modelagem econométrica")

# ============================================================================

# --------- Local Projections ------------------------------------------------
estimar_lp_irf <- function(acao, selecao, metodo, ajuste) {
  
  # 0) Pacotes ---------------------------------------------------------------
  suppressPackageStartupMessages({
    library(readxl)
    library(vars)
    library(lpirfs)
    library(dplyr)
  })
  

  # 1) Leitura dos dados -----------------------------------------------------
  caminho <- sprintf("resultados do LASSO/%s - total - %s - %s - %s - variáveis_selecionadas.xlsx",
                     selecao, metodo, ajuste, acao)
  dados <- read_excel(caminho)
  
  # 2) Preparação das séries -------------------------------------------------
  var_resp <- "Ação_da_Petrobras"
  vars_expl <- setdiff(names(dados), var_resp)
  
  endog_data <- dados # Já vem ordenados do LASSO..!
  
  # 3) VAR intermediário para escolher p* via AIC ----------------------------
  max_p <- 12L
  aics <- sapply(1:max_p, function(p) AIC(VAR(endog_data, p = p, type = "const")))
  p_opt <- which.min(aics)
  
  # 4) Local Projections -----------------------------------------------------
  modelo_lp <- lp_lin(
    endog_data     = endog_data,
    lags_endog_lin = p_opt,   # número ótimo de lags
    hor            = 4*3,      # horizonte IRF
    trend          = 0,       # sem tendência
    shock_type     = 1,       # choque de 1 desvio-padrão
    confint        = 1.96,    # IC 95 %
    use_nw         = TRUE,    # Newey-West
    num_cores      = parallel::detectCores(logical = FALSE)
  )
  
  # 5) Transformação das IRFs em data-frame longo ---------------------------
  irfs_para_df <- function(mod, tipo = c("mean", "low", "up")) {
    tipo <- match.arg(tipo)
    array_irf <- switch(tipo,
                        mean = mod$irf_lin_mean,
                        low  = mod$irf_lin_low,
                        up   = mod$irf_lin_up)
    
    n_var <- dim(array_irf)[1]
    n_h   <- dim(array_irf)[2]
    nomes <- colnames(mod$specs$y_lin)
    
    expand.grid(
      resposta = nomes,
      horizonte = 0:(n_h - 1),
      choque = nomes,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ) |>
      mutate(valor = as.vector(array_irf),
             .before = 1) |>
      rename(variavel = resposta,
             variavel_choque = choque)
  }
  
  df_mean  <- irfs_para_df(modelo_lp, "mean")
  df_low   <- irfs_para_df(modelo_lp, "low")
  df_up    <- irfs_para_df(modelo_lp, "up")
  
  df_irfs <- df_mean |>
    mutate(lower = df_low$valor,
           upper = df_up$valor)
  
  # 6) Saída -----------------------------------------------------------------
  return(list(modelo_lp = modelo_lp,
              irfs      = df_irfs))
}

# ============================================================================

# --------- Pacotes ----------------------------------------------------------
suppressPackageStartupMessages(library(writexl))   # escrever .xlsx sem avisos

# --------- Diretório de saída -----------------------------------------------
out_dir <- "resultados das Local Projections"
dir.create(out_dir, showWarnings = FALSE)          # cria a pasta se faltar

# --------- Indexadores (ajuste livremente) ----------------------------------
selecoes <- c("valor")
#"globorural", "epocanegocios", "valorinveste", "revistapegn"
metodos  <- c("batch", "online")                   
ajustes  <- c("padrão", "ajustado")               
acoes    <- c("PETR4", "PETR3")                   

# --------- Loop principal ---------------------------------------------------
for (selecao in selecoes)           # portal (externo)
  for (acao in acoes)               # ticker
    for (metodo in metodos)         # LDA (batch/online)
      for (ajuste in ajustes)       # ajuste de texto
        tryCatch({
          
          # --- 1) Estimar Local Projections ---------------------------------
          res <- estimar_lp_irf(acao, selecao, metodo, ajuste)
          
          # --- 2) Montar o caminho de gravação ------------------------------
          caminho_xlsx <- sprintf(
            "%s/resultados - %s - %s - %s - %s.xlsx",
            out_dir, selecao, metodo, ajuste, acao
          )
          
          # --- 3) Escrever as IRFs no Excel ---------------------------------
          write_xlsx(res$irfs, path = caminho_xlsx)
          
        }, error = function(e) {})  # silencia falhas e continua
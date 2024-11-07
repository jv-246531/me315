library(tidyverse)
library(dplyr)
library(future.apply)  # For cross-platform parallelism

cria_banco_de_dados <- function(ano = 2023,
                                colunas_selecionar,
                                colunas_tipos) {
  
  arquivo <- paste0("arquivos/", ano, ".zip")
  colunas_tipos <- strsplit(colunas_tipos, "")[[1]]
  logica_tipos <- setNames(as.list(colunas_tipos), colunas_selecionar)
  
  # Unzipping files only once
  arquivos <- unzip(arquivo, list = TRUE)$Name
  
  handler <- function(file) {
    con <- unz(arquivo, file)
    
    # Read the first 6 lines quickly
    informacoes_gerais <- read_lines(con, n_max = 6) %>% 
      str_split_fixed(";", 2) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% 
      t() %>% 
      as.data.frame()
    
    colnames(informacoes_gerais) <- informacoes_gerais[1, ]
    informacoes_gerais <- informacoes_gerais[-1, ]
    
    # Read the time series data with specified types
    serie_temporal <- read_delim(unz(arquivo, file),
                                 skip = 8,
                                 locale = locale(encoding = "ISO-8859-1",
                                                 decimal_mark = ","),
                                 delim = ";",
                                 col_select = all_of(colunas_selecionar),
                                 col_types = logica_tipos,
                                 show_col_types = FALSE)
    
    serie_temporal <- serie_temporal %>%
      mutate(estacao = informacoes_gerais["ESTACAO:"] %>% pull)
    
    return(serie_temporal)
  }
  
  # Set up parallel plan (will use multi-core on Linux/macOS and multi-session on Windows)
  plan(multisession) 
  
  # Use future_lapply for parallel processing
  dados <- future_lapply(arquivos, handler) %>%
    bind_rows()
  
  return(dados)
}

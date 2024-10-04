library(tidyverse)
library(dplyr)

cria_banco_de_dados <- function(ano = 2023,
                                colunas_selecionar,
                                colunas_tipos) {
  
  arquivo <- paste0("arquivos/", ano, ".zip")
  
  colunas_tipos <- strsplit(colunas_tipos, "")[[1]]
  
  logica_tipos <- setNames(as.list(colunas_tipos), colunas_selecionar)
  
  handler <- function(file) {
    serie_temporal <- read_delim(unz(arquivo,
                                     file),
                                 skip = 8,
                                 locale = locale(encoding = "ISO-8859-1",
                                                 decimal_mark = ","),
                                 delim = ";",
                                 col_select = all_of(colunas_selecionar),
                                 col_types = logica_tipos,
                                 show_col_types = FALSE
                                 
    )
    
    informacoes_gerais <- read_delim(unz(arquivo,
                                         file),
                                     n_max = 6,
                                     locale = locale(encoding = "ISO-8859-1",
                                                     decimal_mark = ","),
                                     delim = ";",
                                     col_names = c(1,2),
                                     show_col_types = FALSE
    ) %>% t() %>% as.data.frame()
    
    colnames(informacoes_gerais) <- informacoes_gerais[1,]
    informacoes_gerais <- informacoes_gerais[-1,]
    
    serie_temporal <- serie_temporal %>%
      mutate(estacao = informacoes_gerais["ESTACAO:"] %>% pull)
    
    return(serie_temporal)
    
  }
  
  
  dados <- unzip(arquivo, list = TRUE)$Name %>%
    lapply(handler) %>%
    bind_rows
  
  return(dados)
}

# EXEMPLO

#ahslaoq <- cria_banco_de_dados(2020,
#                         c("Data",
#                           "Hora UTC",
#                           "TEMPERATURA DO PONTO DE ORVALHO (°C)"),
#                         "Dcd")
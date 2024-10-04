library(tidyverse)
library(dplyr)



handler <- function(file) {
  serie_temporal <- read_delim(unz("arquivos/2023.zip",
                                 file),
                             skip = 8,
                             locale = locale(encoding = "ISO-8859-1",
                                             decimal_mark = ","),
                             delim = ";",
                             col_select = c("Data",
                                            "Hora UTC",
                                            "TEMPERATURA DO PONTO DE ORVALHO (°C)"),
                             col_types = cols(
                               Data = "D", 
                               `Hora UTC` = "c", 
                               `TEMPERATURA DO PONTO DE ORVALHO (°C)` = "d"
                             ),
                             show_col_types = FALSE
                             
  )
  
  informacoes_gerais <- read_delim(unz("arquivos/2023.zip",
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


dados <- unzip("arquivos/2023.zip", list = TRUE)$Name %>%
  lapply(handler) %>%
  bind_rows

sla_exemplo <- dados %>%
  filter(complete.cases(.)) %>%
  group_by(estacao) %>%
  summarise(media = mean(`TEMPERATURA DO PONTO DE ORVALHO (°C)`))
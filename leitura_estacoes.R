library(tidyverse)
library(dplyr)



dados <- unzip("arquivos/2023.zip", list = TRUE)$Name %>%
  lapply(function(file) {
    informacoes_gerais <- read_delim(unz("arquivos/2023.zip",
                   file),
               n_max = 6,
               locale = locale(encoding = "ISO-8859-1",
                               decimal_mark = ","),
               delim = ";",
               col_names = c(1,2),
               show_col_types = FALSE
    ) %>% t() %>% as.data.frame()
    
    colnames(informacoes_gerais) <- substr(informacoes_gerais[1,],
                                           1,
                                           nchar(informacoes_gerais[1,]) -1)
    informacoes_gerais <- informacoes_gerais[-1,]
    
    return(informacoes_gerais)
  }) %>%
  bind_rows %>%
  mutate(LATITUDE = as.numeric(gsub(",", ".", LATITUDE)),
         LONGITUDE = as.numeric(gsub(",", ".", LONGITUDE))) %>%
  saveRDS("novosbancos/cidades.rds")

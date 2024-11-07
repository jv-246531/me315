library(dplyr)
library(tidyverse)

if (!exists("nomes_colunas")) {
  nomes_colunas <- readRDS("novosbancos/dados2024.rds") %>% colnames
}


if (!exists("dados_chuva_umidade")) {
  dados_chuva_umidade <- tibble()
  
  
  for (ano in 2015:2024) {
    parcial <- readRDS(paste0("novosbancos/dados", ano, ".rds"))
    gc()
    
    colnames(parcial) <- nomes_colunas
    
    parcial <- parcial %>%
      select(Data, `Hora UTC`, `PRECIPITAÇÃO TOTAL, HORÁRIO (mm)`, `UMIDADE RELATIVA DO AR, HORARIA (%)`, estacao) %>%
      mutate_if(is.numeric, ~ ifelse(. == -9999, NA, .)) %>%
      filter(complete.cases(.)) %>%
      rename(umidade = `UMIDADE RELATIVA DO AR, HORARIA (%)`,
             precipitacao = `PRECIPITAÇÃO TOTAL, HORÁRIO (mm)`) %>%
      group_by(Data, estacao) %>%
      summarise(media_umi = mean(umidade),
                total_prec = sum(precipitacao)) %>%
      inner_join(cidades, by = c("estacao" = "ESTACAO")) %>%
      select(Data, estacao, media_umi, total_prec, REGIAO) %>%
      mutate(mes = my(format(Data, "%m-%Y"))) %>%
      group_by(mes, REGIAO) %>%
      summarise(media_umidade_mes = mean(media_umi),
                media_prec_por_estacao = mean(total_prec)*30)
    
    dados_chuva_umidade <- rbind(dados_chuva_umidade, parcial)
    
    rm(parcial)
    print(paste0(ano, " / 2024"))
    gc()
  }
}

grafico <- ggplot(dados_chuva_umidade) +
  geom_line(aes(x = mes,
                y = media_prec_por_estacao,
                group = REGIAO,
                colour = REGIAO)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Year") +
  theme_classic()


dados <- dados_chuva_umidade %>%
  mutate(mes = format(mes, "%m"))

modelo <- lm(data = dados, media_umidade_mes ~ sqrt(media_prec_por_estacao) + mes*REGIAO)

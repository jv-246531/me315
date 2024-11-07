library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "novosbancos/dados.sqlite")

dbWriteTable(conn, name = "cidades", readRDS("novosbancos/cidades.rds"), overwrite = TRUE)
for (ano in 2000:2024) {
  dbWriteTable(conn,
               name = paste0("dados", ano),
               as.data.frame(readRDS(paste0("novosbancos/dados", ano, ".rds"))),
               overwrite = TRUE)
}

dbDisconnect(conn)
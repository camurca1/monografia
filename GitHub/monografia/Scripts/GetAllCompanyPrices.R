rm(list = ls())
options(scipen = 999)


library(BatchGetSymbols)
library(lubridate)

cia.info <- readRDS("Data/cia_info_reduzido")
tickers <- unlist((strsplit(c(t(na.omit(cia.info$tickers))), ";")))

#### Importar dados financeiros ####

# Definir parametros
papel <- paste0(tickers, ".SA")
dt_inicial <- ymd("2014-01-01")
dt_final <- ymd("2020-12-31")
thresh.bad.data <- 0.95   # filtro de qualidade de dados
bench.ticker <- '^BVSP'   # ibov como benchmark
cache.folder <- 'Data/BGS_Cache' # cache de memoria

# importar dados financeiros
ls_papel <- BatchGetSymbols(papel, dt_inicial, dt_final, thresh.bad.data, bench.ticker)
df_papel <- ls_papel$df.tickers
df_papel$ticker <- gsub(".SA", "", df_papel$ticker)
df_controle <- ls_papel$df.control

saveRDS(tickers, "Data/tickers")
saveRDS(df_papel, "Data/precos_acoes")
saveRDS(df_controle, "Data/acoes_controle")

rm(list = ls())
gc()

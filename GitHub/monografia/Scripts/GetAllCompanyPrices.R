rm(list = ls())
options(scipen = 999)


library(BatchGetSymbols)
library(lubridate)

cia.info <- readRDS("Data/cia_info_reduzido")
tickers <- readRDS("Data/tabelatickers")

#### Importar dados financeiros ####

# Definir parametros
papel <- paste0(tickers$simbolo, ".SA")
dt_inicial <- ymd("2014-01-01")
dt_final <- ymd("2020-12-31")
thresh.bad.data <- 0.95   # filtro de qualidade de dados
bench.ticker <- '^BVSP'   # ibov como benchmark
cache.folder <- 'Data/BGS_Cache' # cache de memoria

# importar dados financeiros
ls_papel <- BatchGetSymbols(papel, dt_inicial, dt_final, thresh.bad.data, bench.ticker)
df_papel <- ls_papel$df.tickers
df_papel$ticker <- gsub("\\.SA", "", df_papel$ticker)
df_controle <- ls_papel$df.control
df_controle$ticker <- gsub("\\.SA", "", df_controle$ticker)

# importar benchmark
ls_ibov <- BatchGetSymbols("^BVSP", dt_inicial, dt_final, thresh.bad.data, bench.ticker)
df_ibov <- ls_ibov$df.tickers
df_ibov$ticker <- gsub("\\.SA", "", df_ibov$ticker)

# tratamento tabela tickers
tickers <- left_join(tickers,
                    select(df_controle,
                           ticker,
                           threshold.decision),
                    by = c("simbolo" = "ticker"))
tickers <- tickers %>%
  na.omit(threshold.decision) %>%
  filter(threshold.decision != "OUT")
tickers$threshold.decision <- NULL

# salvar arquivos
saveRDS(df_papel, "Data/precos_acoes")
saveRDS(df_controle, "Data/acoes_controle")
saveRDS(tickers, "Data/tabelatickers")
saveRDS(df_ibov, "Data/precosibov")
rm(df_controle, df_papel, df_ibov)

# selecionar acoes mais negociadas
preco.papel <- readRDS("Data/precos_acoes")
preco.papel.agregado <- aggregate(preco.papel$volume,
                                  by = list(Tickers=preco.papel$ticker),
                                  FUN = mean)
names(preco.papel.agregado)[2] <- "Volume Médio"
tickers.negociados <- tickers
tickers.negociados <- left_join(tickers.negociados,
                                preco.papel.agregado,
                                by=c("simbolo" = "Tickers"))
tickers.negociados <- tickers.negociados %>%
  group_by(CD_CVM) %>%
  top_n(1, `Volume Médio`)

saveRDS(tickers.negociados, "Data/tickers_mais_negociados")

rm(list = ls())
gc()


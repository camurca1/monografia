# Escrito por: Alexandre Camurça Silva de Souza
# Ambiente RStudio Desktop 1.4.1717 "Juliet Rose"

# Etapa 6 - Elaboração do Ranking TOPSIS


# limpar memória e desativar notação científica
rm(list = ls())
options(scipen = 999)

#### Gereciamento de pacotes ####

# informar os pacotes que serao utilizados no script
pacotes <- c("topsis", "bizdays", "dplyr", "lubridate")

# instalar pacotes ausentes
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(c(pacotes[!pacotes_instalados]))
}

# carregar pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


#### criar calendário de dias úteis ####
holidaysANBIMA <- append(holidaysANBIMA,  ymd(c("2016-12-30", "2017-12-29")))
cal <- create.calendar("Brazil/ANBIMA",
                       holidaysANBIMA, weekdays=c("saturday", "sunday"))


#### carregar tabelas salvas ####
ind.fundamento <- (readRDS("Data/indices_calculados"))
ind.tecnico <- (readRDS("Data/indAT"))
bp <- (readRDS("Data/balanco_patrimonial"))


#### criar matriz de decisão ####
amd.matriz <- subset(ind.fundamento,
                     ind.fundamento$DT_REFER == "2015-12-31")
amd.matriz$dia.util.ant <- add.bizdays(amd.matriz$DT_REFER, -1, cal)
amd.matriz <- left_join(amd.matriz,
                        select(ind.tecnico, CD_CVM, ticker,
                               ref.date, sinal.decisao),
                        by=(c("CD_CVM", "ticker",
                              "dia.util.ant" = "ref.date")))
row.names(amd.matriz) <- amd.matriz$ticker
amd.matriz$ticker <- NULL
amd.matriz$CD_CVM <- NULL
amd.matriz$DT_REFER <- NULL
amd.matriz$dia.util.ant <- NULL
amd.matriz <- as.matrix(amd.matriz)

rm(ind.fundamento, ind.tecnico)
gc()

#### definir parametros dos critérios ####
pesos <- c(rep(1/ncol(amd.matriz), ncol(amd.matriz)))
critPosNeg <- c("+", "-", "+", "+", "+", "+", "+")


#### construir ranking ####
ranking <- topsis(amd.matriz, pesos, critPosNeg)
ranking$ticker <- row.names(amd.matriz)
ranking$alt.row <- NULL
ranking <- unique(left_join(ranking,
                            select(bp, simbolo, sub.sector),
                            by = c("ticker" = "simbolo")))
top.30 <- slice_max(ranking, order_by = ranking$score, n = 30)

rm(bp, amd.matriz)
gc()

#### extrair carteiras do ranking ####
carteira1 <- slice_max(ranking, order_by = ranking$score, n = 15)
carteira1$pesos <- 1/nrow(carteira1)
carteira2 <- slice_min(ranking, order_by = ranking$score, n = 15)
carteira2$pesos <- 1/nrow(carteira2)
carteira3 <- top.30 %>%
  group_by(sub.sector) %>%
  slice_max(order_by = score, n = 1)
carteira3$pesos <- 1/nrow(carteira3)

l.carteiras <- list("carteira1" = carteira1,
                    "carteira2" = carteira2,
                    "carteira3" = carteira3)

saveRDS(l.carteiras, "Data/carteirasTOPSIS")
saveRDS(ranking, "Data/rankingTOPSIS")

# write.csv2(l.carteiras, "Data/carteirasTOPSIS.csv", row.names = F)
# write.csv2(ranking, "Data/ranking_ativos.csv", row.names = F)

# limpeza de memória
rm(list = ls())
gc()

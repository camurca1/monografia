rm(list = ls())
options(scipen = 999)
set.seed(10)


library(topsis)
library(bizdays)
library(dplyr)

holidaysANBIMA <- append(holidaysANBIMA,  ymd(c("2016-12-30", "2017-12-29")))
cal <- create.calendar("Brazil/ANBIMA",
                       holidaysANBIMA, weekdays=c("saturday", "sunday"))

ind.fundamento <- (readRDS("Data/indices_calculados"))
ind.tecnico <- (readRDS("Data/indAT"))
bp <- (readRDS("Data/balanco_patrimonial"))



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

pesos <- c(rep(1/ncol(amd.matriz), ncol(amd.matriz)))
critPosNeg <- c("+", "-", "+", "+", "+", "+", "+")

ranking <- topsis(amd.matriz, pesos, critPosNeg)
ranking$ticker <- row.names(amd.matriz)
ranking$alt.row <- NULL
ranking <- unique(left_join(ranking,
                            select(bp, simbolo, sub.sector),
                            by = c("ticker" = "simbolo")))
top.30 <- slice_max(ranking, order_by = ranking$score, n = 30)

rm(bp, amd.matriz)
gc()

carteira1 <- top.30 %>%
  group_by(sub.sector) %>%
  slice_max(order_by = score, n = 1)
carteira2 <- slice_max(ranking, order_by = ranking$score, n = 15)
carteira3 <- slice_min(ranking, order_by = ranking$score, n = 15)
carteira4 <- sample_n(ranking, 15)

l.carteiras <- list("carteira1" = carteira1,
                    "carteira2" = carteira2,
                    "carteira3" = carteira3,
                    "carteira4" = carteira4)

saveRDS(l.carteiras, "Data/carteirasTOPSIS")
saveRDS(ranking, "Data/rankingTOPSIS")

rm(list = ls())
gc()

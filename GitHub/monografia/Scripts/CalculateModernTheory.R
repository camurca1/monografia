rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(fPortfolio)


ret.vol.acoes <- readRDS("Data/retorno_volatilidade_acoes")

precos.acoes <- as.data.frame(ret.vol.acoes$CD_CVM)
precos.acoes$ticker <- ret.vol.acoes$ticker
precos.acoes$ref.date <- ret.vol.acoes$ref.date
precos.acoes$retornos <- ret.vol.acoes$retornos
precos.acoes <- subset(precos.acoes, precos.acoes$ref.date < "2016-01-01")

retornos.matriz <- precos.acoes
names(retornos.matriz)[1] <- "CD_CVM"
retornos.matriz$CD_CVM <- NULL
retornos.matriz$`ret.vol.acoes$CD_CVM` <- NULL
retornos.matriz$ticker <- as.factor(retornos.matriz$ticker)
retornos.matriz <- retornos.matriz %>%
  group_by(ticker) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = ticker, values_from = retornos, values_fill = 0) %>%
  select(-row)
retornos.matriz$ref.date <- NULL
retornos.matriz <- as.timeSeries(retornos.matriz)

specs <- portfolioSpec()
`setNFrontierPoints<-`(specs, 1000)

portfolio.eficiente <- tangencyPortfolio(retornos.matriz, spec = specs, constraints = "LongOnly")
weightsPie(portfolio.eficiente, col = qualiPalette(n = 76, "Set1"))
mtext(text = "Pesos - Portifolio Eficiente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)


fronteira <- portfolioFrontier(retornos.matriz, spec = specs, constraints = "LongOnly")


frontierPlot(fronteira, col = c('blue', 'red'), pch = 20, frontier = "upper",
             ylim = c(0, 0.002), type = "l")
grid()
minvariancePoints(fronteira,col="red",pch=20, return = "mean", risk = "Cov")
tangencyPoints(fronteira,col="blue",pch=20)
tangencyLines(fronteira, col="green")
sharpeRatioLines(object=fronteira, return ="mean",
                 risk =  "Cov", pch=20)
xy <- singleAssetPoints(fronteira, return = "mean", risk = "Cov",
                        auto = FALSE, cex = 1.5,
                        col = rainbow(6), lwd = 2,
                        ylim = c(0, 0.002))
singleAssetPoints(fronteira, return = "mean", risk = "Cov",
                       auto = FALSE, cex = 1.5,
                       col = rainbow(6), lwd = 2,
                  ylim = c(0, 0.002),
                  text(xy[, 1], xy[, 2], pos = 3, rownames(xy), font = 2, cex = 0.7))

dados.portifolio <- getPortfolio(portfolio.eficiente)
carteira <- as.data.frame(dados.portifolio$weights)
carteira$Tickers <- row.names(carteira)
row.names(carteira) <- NULL
carteira <- subset(carteira, carteira$`dados.portifolio$weights` > 0)
names(carteira)[1] <- "Pesos"
carteira <- carteira %>% select(Tickers, Pesos)

saveRDS(dados.portifolio, "Data/dados_carteira_eficiente")
saveRDS(carteira, "Data/carteira_eficiente")
write.csv2(carteira, "Data/carteira_eficiente.csv", row.names = F)

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
`setRiskFreeRate<-`(specs, 3)
`setNFrontierPoints<-`(specs, 100)

portfolio.eficiente <- tangencyPortfolio(retornos.matriz, spec = specs, constraints = "LongOnly")

fronteira <- portfolioFrontier(retornos.matriz)

frontierPlot(fronteira, col = c('blue', 'red'), pch = 20)
monteCarloPoints(fronteira, mcSteps = 5000, pch = 20, cex = 0.25 )

dados.portifolio <- getPortfolio(portfolio.eficiente)

saveRDS(dados.portifolio, "Data/carteira_eficiente")

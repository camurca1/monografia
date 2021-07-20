rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(lubridate)
library(fGarch)
library(forecast)
library(tseries)
library(FinTS)


log_retorno <- function(x){
  x$retornos <- c(as.numeric(0),
                  diff(log(x$price.adjusted), lag=1, differences = 1))
  return(x)
}

volatilidade <- function(x){
  x$volatilidade <- volatility(garchFit(data = x$retornos,
                                        formula = ~arma(0,0)+garch(1,1),
                                        trace = F))
  return(x)
}

bp <- (readRDS("Data/balanco_patrimonial"))
precos.empresas <- as_tibble(readRDS("Data/precos_acoes"))
precos.empresas$ref.date <- ymd(precos.empresas$ref.date)


precos <- as.data.frame(bp$CD_CVM)
names(precos)[1] <- "CD_CVM"
precos$ticker <- bp$simbolo
precos <- unique(precos)
precos <-  left_join(precos,
                     select(precos.empresas,
                            ticker,
                            ref.date,
                            price.adjusted),
                     by = "ticker")
precos$retornos <- NA
precos$volatilidade <- NA

l.acoes <- split(precos, precos$ticker)
l.acoes <- lapply(l.acoes, log_retorno)
l.acoes <- lapply(l.acoes, volatilidade)

precos <- unsplit(l.acoes, precos$ticker)

saveRDS(precos, "Data/retorno_volatilidade_acoes")

rm(list = ls())
gc()

#### Código utilizado para o teste do modelo ####

# ABEV3 <- subset(precos, precos$ticker == "ELET3")
#
# modelo1 <- auto.arima(CMIG4$retornos, stationary = T)
#
#
# summary(modelo1)
# par(mfrow=c(2,1))
# acf(modelo1$residuals)
# plot(modelo1$residuals)
# tsdiag(modelo1)
# Box.test(modelo1$residuals, type =  "Ljung-Box", lag = 30)
# adf.test(CMIG4$retornos)
# par(mfrow=c(2,1))
# acf((modelo1$residuals)^2)
# pacf((modelo1$residuals)^2)
# Box.test((modelo1$residuals)^2, type =  "Ljung-Box", lag = 30)
# ArchTest(modelo1$residuals, lags = 30, demean = F)
#
#
#
# my_garchfit <- garchFit(data = CMIG4$retornos, formula = ~arma(0,0)+garch(1,1), trace = F)
# CMIG4$volatilidade <- volatility(my_garchfit)
#
# fGarch::formula(my_garchfit)
# fGarch::fitted(my_garchfit)
# fGarch::coef(my_garchfit)
# volatility(my_garchfit)
# fGarch::predict(my_garchfit)
# summary(my_garchfit)
#
# CMIG4$fitted <- fGarch::fitted(my_garchfit)

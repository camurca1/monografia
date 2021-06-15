rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(tidyquant)
library(tidyr)
library(lubridate)
library(forecast)
library(fGarch)
library(FinTS)

log_retorno <- function(x){
  x$retornos <- c(as.numeric(0), diff(log(x$price.close), lag=1, differences = 1))
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
                            price.close),
                     by = "ticker")
precos$retornos <- NA
precos$volatilidade <- NA

l.acoes <- split(precos, precos$ticker)
l.acoes <- lapply(l.acoes, log_retorno)
l.acoes <- lapply(l.acoes, volatilidade)
precos <- unsplit(l.acoes, precos$ticker)

saveRDS(precos, "Data/retorno_volatilidade_acoes")
#### Código utilizado para o teste do modelo ####

# modelo1 <- auto.arima(ELET3$retornos, max.p=6, max.q = 6, max.d = 1, max.P=8, max.D=1,
#                       max.Q=8, start.p = 1, lambda = NULL, stationary = F,
#                       stepwise = F, allowdrift = T, allowmean = T, trace = F)
#
# summary(modelo1)
# par(mfrow=c(2,1))
# acf(modelo1$residuals)
# plot(modelo1$residuals)
# tsdiag(modelo1)
# Box.test(modelo1$residuals, type =  "Ljung-Box", lag = 24)
#
# par(mfrow=c(2,1))
# acf((modelo1$residuals)^2)
# pacf((modelo1$residuals)^2)
# Box.test((modelo1$residuals)^2, type =  "Ljung-Box", lag = 5)
# ArchTest(modelo1$residuals, lags = 5, demean = F)

# GARCH_1_1 <- ugarchspec(variance.model = list(model = "sGARCH",
#                                               garchOrder = c(1,1),
#                                               submodel = NULL,
#                                               external.regressors = NULL,
#                                               variance.targeting = F),
#                         mean.model = list(armaOrder=c(0,0),
#                                           include.mean = T,
#                                           archm = F,
#                                           archpow = 1,
#                                           arfima = F,
#                                           archex = F),
#                         distribution.model = "norm")
# mod_GARCH_1 <- ugarchfit(GARCH_1_1, data = ELET3$retorno, out.sample = 10)
# print(mod_GARCH_1)
# plot(mod_GARCH_1, which="all")

# my_garchfit <- volatility(garchFit(data = x$retornos, formula = ~arma(0,0)+garch(1,1), trace = F))


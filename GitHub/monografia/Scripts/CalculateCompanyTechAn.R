rm(list = ls())
options(scipen = 999)

library(pracma)
library(RcppRoll)
library(tidyverse)

calc_MME <- function(x){
  x$MME.lenta <- movavg(x$price.adjusted, 150, "e")
  x$MMS.rapida <- movavg(x$price.adjusted, 50, "s")
  return(x)
}

calc_MACD <- function(x){
  x$MACD1 <- (movavg(x$price.adjusted, 26, "e")- movavg(x$price.adjusted, 12, "e"))
  x$MACD2 <- movavg(x$MACD1, 9, "e")
  return(x)
}

calc_est <- function(x){
  x$price.adjusted.min <- roll_min(x$price.adjusted,
                                       n = 14,
                                       fill = NA,
                                       align = "right")
  x$price.adjusted.max <- roll_max(x$price.adjusted,
                                       n = 14,
                                       fill = NA,
                                       align = "right")
  x$estK <- (x$price.adjusted - x$price.adjusted.min)/(x$price.adjusted.max - x$price.adjusted.min)
  x$estD <- movavg(x$estK, 3, "s")
  return(x)
}

precos.empresas <- readRDS("Data/precos_acoes")
precos.analise <- readRDS("Data/retorno_volatilidade_acoes")

l.precos <- split(precos.analise, precos.analise$ticker)
l.precos <- lapply(l.precos, calc_MME)
l.precos <- lapply(l.precos, calc_MACD)
l.precos <- lapply(l.precos, calc_est)
precos.analise <- unsplit(l.precos, precos.analise$ticker)

ELET3 <- subset(precos.analise, precos.analise$ticker == "ELET3")
ELET3$mov.MM <- if_else(ELET3$MMS.rapida > ELET3$MME.lenta, 1,
                         if_else(ELET3$MMS.rapida < ELET3$MME.lenta, -1, 0, missing = 0),
                         missing = 0)
ELET3$mov.MACD <- if_else(ELET3$MACD1 > ELET3$MACD2, 1, -1, missing = 0)
ELET3$mov.est <- if_else(ELET3$estK > 0.8 & ELET3$estD > 0.8,
                         -1,
                         if_else(ELET3$estK < 0.2 & ELET3$estD < 0.2,
                                 1,
                                 0,
                                 missing = 0),
                         missing = 0)

plot(ELET3$ref.date, ELET3$price.adjusted, type = "l")
plot(ELET3$ref.date, ELET3$mov.MME, type = "l")

saveRDS(precos.analise, "Data/indAT")


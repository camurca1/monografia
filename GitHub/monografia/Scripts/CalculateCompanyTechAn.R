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

calc_decisaoCV <- function(x){
  x$mov.MM <- if_else(x$MMS.rapida > x$MME.lenta, 1,
                          if_else(x$MMS.rapida < x$MME.lenta, -1, 0, missing = 0),
                          missing = 0)
  x$mov.MACD <- if_else(x$MACD1 > x$MACD2, 1, -1, missing = 0)
  x$mov.est <- if_else(x$estK > 0.8 & x$estD > 0.8,
                           -1,
                           if_else(x$estK < 0.2 & x$estD < 0.2,
                                   1,
                                   0,
                                   missing = 0),
                           missing = 0)
  x$aux.sinal.MM <- roll_sum(x$mov.MM, n = 3, align = "right", fill = 0)
  x$aux.sinal.MACD <- roll_sum(x$mov.MACD, n = 3, align = "right", fill = 0)
  x$sinal.MM <- if_else(x$aux.sinal.MM == 3,
                            1,
                            if_else(x$aux.sinal.MM == -3,
                                    1,
                                    0,
                                    missing = 0),
                            missing = 0)
  x$sinal.MACD <- if_else(x$aux.sinal.MACD == 3,
                              1,
                              if_else(x$aux.sinal.MACD == -3,
                                      -1,
                                      0,
                                      missing = 0),
                              missing = 0)
  x$aux.sinal.decisao <- x$sinal.MACD + x$sinal.MM + x$mov.est
  x$sinal.decisao <- if_else(x$aux.sinal.decisao > 1,
                                 1,
                                 if_else(x$aux.sinal.decisao < -1,
                                         -1,
                                         0,
                                         missing = 0),
                                 missing = 0)
  x$decisao.final <- if_else(x$sinal.decisao == 1,
                                 "COMPRA",
                                 if_else(x$sinal.decisao == -1,
                                         "VENDA",
                                         "NEUTRO",
                                         missing = "NEUTRO"),
                                 missing = "NEUTRO")
  x[ ,5:20] <- NULL
  return(x)
}

precos.empresas <- readRDS("Data/precos_acoes")
precos.analise <- readRDS("Data/retorno_volatilidade_acoes")
precos.analise$retornos <- NULL
precos.analise$volatilidade <- NULL

l.precos <- split(precos.analise, precos.analise$ticker)
l.precos <- lapply(l.precos, calc_MME)
l.precos <- lapply(l.precos, calc_MACD)
l.precos <- lapply(l.precos, calc_est)
l.precos <- lapply(l.precos, calc_decisaoCV)
precos.analise <- unsplit(l.precos, precos.analise$ticker)

saveRDS(precos.analise, "Data/indAT")


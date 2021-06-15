rm(list = ls())
options(scipen = 999)

library(pracma)
library(RcppRoll)

calc_MME <- function(x){
  x$MME <- movavg(x$price.adjusted, 26, "e")
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

saveRDS(precos.analise, "Data/indAT")

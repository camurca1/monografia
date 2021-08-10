# Escrito por: Alexandre Camurça Silva de Souza
# Ambiente RStudio Desktop 1.4.1717 "Juliet Rose"

# Etapa 4 - Calcular retornos e volatilidades das ações


# limpar memória e desativar notação científica
rm(list = ls())
options(scipen = 999)

#### Gereciamento de pacotes ####

# informar os pacotes que serao utilizados no script
pacotes <- c("tidyverse", "lubridate", "fGarch", "forecast", "tseries", "FinTS")

# instalar pacotes ausentes
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(c(pacotes[!pacotes_instalados]))
}

# carregar pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


#### funções para calculo de log-retorno e volatilidade ####
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


#### carregar tabelas salvas ####
bp <- readRDS("Data/balanco_patrimonial")
indices <- readRDS("Data/indices_calculados")
precos.empresas <- as_tibble(readRDS("Data/precos_acoes"))
precos.empresas$ref.date <- ymd(precos.empresas$ref.date)


#### calcular retornos e volatilidades ####
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

volatilidade.empresas <- tibble(precos)

media.volatilidade <- subset(volatilidade.empresas,
                             year(volatilidade.empresas$ref.date) > 2014)
media.volatilidade <- media.volatilidade %>%
  group_by(ticker, ano = lubridate::year(ref.date)) %>%
  summarize(., volatilidade.media.ano = mean(volatilidade)) %>%
  mutate(DT_REFER = ymd(paste0(ano, "-12-31")))

media.volatilidade$ano <- NULL
names(media.volatilidade)[1] <- "simbolo"
volatilidade.ano <- as.data.frame(bp$CD_CVM)
volatilidade.ano$DT_REFER <- bp$DT_REFER
volatilidade.ano$simbolo <- bp$simbolo
names(volatilidade.ano)[1:3] <- c("CD_CVM", "DT_REFER", "simbolo")
volatilidade.ano <- left_join(volatilidade.ano,
                              media.volatilidade,
                              c("DT_REFER", "simbolo"))

bp <- left_join(bp, volatilidade.ano, by=c("CD_CVM", "DT_REFER", "simbolo"))
saveRDS(bp, "Data/balanco_patrimonial")

indices$Volatilidade <- bp$volatilidade.media.ano
saveRDS(indices, "Data/indices_calculados")

#### limpeza de memória ####
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

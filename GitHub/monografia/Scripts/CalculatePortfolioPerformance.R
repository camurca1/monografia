rm(list = ls())
options(scipen = 999)

library(dplyr)
library(lubridate)
library(tidyquant)
library(quantmod)
library(ggplot2)


caixa.disponivel <- 5000
data.investimento <- ymd("2015-12-30")
precos.acoes <- readRDS("Data/retorno_volatilidade_acoes")
precos.ibov <- readRDS("Data/precosibov")
precos.acoes$price.adjusted <- round(precos.acoes$price.adjusted, 2)
retorno.mensal.ativos <- precos.acoes %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")
periodo.analise <- subset(precos.acoes,
                          precos.acoes$ref.date > data.investimento)
periodo.analise.ibov <- subset(precos.ibov,
                               precos.ibov$ref.date > data.investimento)

carteira.MT <- readRDS("Data/carteira_eficiente")
Carteiras.TOPSIS <- readRDS("Data/carteirasTOPSIS")
Carteira1.TOPSIS <- Carteiras.TOPSIS$carteira1
Carteira1.TOPSIS <- Carteira1.TOPSIS[,c("ticker", "pesos")]
Carteira2.TOPSIS <- Carteiras.TOPSIS$carteira2
Carteira2.TOPSIS <- Carteira2.TOPSIS[,c("ticker", "pesos")]
Carteira3.TOPSIS <- Carteiras.TOPSIS$carteira3
Carteira3.TOPSIS <- Carteira3.TOPSIS[,c("ticker", "pesos")]

retorno.mensal.ibov <- periodo.analise.ibov %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb")

###### Performance e Visualizacao Carteira Moderna Teoria ######

precos.carteira.MT <- left_join(carteira.MT, select(periodo.analise, ticker,
                                                    ref.date, price.adjusted),
                                by = c("Tickers" = "ticker"))
precos.carteira.MT$Pesos <- NULL

retorno.ativos.carteira.MT <- precos.carteira.MT %>%
  group_by(Tickers) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")

retorno.portifolio.MT <- retorno.ativos.carteira.MT %>%
  tq_portfolio(assets_col = Tickers,
               returns_col = Ra,
               weights = carteira.MT,
               col_rename = "Ret.MT")

crescimento.portifolio.MT <- retorno.ativos.carteira.MT %>%
  tq_portfolio(assets_col = Tickers,
               returns_col = Ra,
               weights = carteira.MT,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * 10000)

perf.compare.MT <- left_join(retorno.portifolio.MT, retorno.mensal.ibov,
                             by = "ref.date")

retorno.portifolio.MT %>%
  ggplot(aes(x = ref.date, y = Ret.MT)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno do Portifólio Eficiente",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.portifolio.MT %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento Capital",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

###### Performance e Visualizacao Carteira TOPSIS ######

# TOPSIS - 15 primeiros ativos

precos.carteira2.TOPSIS <- left_join(Carteira2.TOPSIS, select(periodo.analise,
                                                             ticker,
                                                             ref.date,
                                                             price.adjusted),
                                by = "ticker")
precos.carteira2.TOPSIS$Pesos <- NULL

retorno.ativos.carteira2.TOPSIS <- precos.carteira2.TOPSIS %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")

retorno.portifolio2.TOPSIS <- retorno.ativos.carteira2.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira2.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio2.TOPSIS <- retorno.ativos.carteira2.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira2.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * 10000)

perf.compare.TOPSIS <- left_join(retorno.portifolio2.TOPSIS, retorno.mensal.ibov,
                             by = "ref.date")

retorno.portifolio2.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno do Portifólio TOPSiS",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.portifolio2.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento Capital - TOPSIS",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

# TOPSIS - 1 ativo de cada setor - melhor pontuação

precos.carteira1.TOPSIS <- left_join(Carteira1.TOPSIS, select(periodo.analise,
                                                              ticker,
                                                              ref.date,
                                                              price.adjusted),
                                     by = "ticker")
precos.carteira1.TOPSIS$Pesos <- NULL

retorno.ativos.carteira1.TOPSIS <- precos.carteira1.TOPSIS %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")

retorno.portifolio1.TOPSIS <- retorno.ativos.carteira1.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira1.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio1.TOPSIS <- retorno.ativos.carteira1.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira1.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * 10000)

perf.compare1.TOPSIS <- left_join(retorno.portifolio1.TOPSIS, retorno.mensal.ibov,
                                 by = "ref.date")

retorno.portifolio1.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno do Portifólio TOPSiS",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.portifolio1.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento Capital - TOPSIS",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

# TOPSIS - 15 piores ativos

precos.carteira3.TOPSIS <- left_join(Carteira3.TOPSIS, select(periodo.analise,
                                                              ticker,
                                                              ref.date,
                                                              price.adjusted),
                                     by = "ticker")
precos.carteira3.TOPSIS$Pesos <- NULL

retorno.ativos.carteira3.TOPSIS <- precos.carteira3.TOPSIS %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")

retorno.portifolio3.TOPSIS <- retorno.ativos.carteira3.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira3.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio3.TOPSIS <- retorno.ativos.carteira3.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = Carteira3.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * 10000)

perf.compare3.TOPSIS <- left_join(retorno.portifolio3.TOPSIS, retorno.mensal.ibov,
                                  by = "ref.date")

retorno.portifolio3.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno do Portifólio TOPSiS 3",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.portifolio3.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento Capital - TOPSIS 3",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

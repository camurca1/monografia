# Escrito por: Alexandre Camurça Silva de Souza
# Ambiente RStudio Desktop 1.4.1717 "Juliet Rose"

# Etapa 8 - Comparação das performances


# limpar memória e desativar notação científica
rm(list = ls())
options(scipen = 999)

# fixar semente aleatória
set.seed(10)

#### Gereciamento de pacotes ####

# informar os pacotes que serao utilizados no script
pacotes <- c("tidyr", "dplyr", "lubridate", "tidyquant", "quantmod", "gridExtra",
             "gtable", "ggplot2")

# instalar pacotes ausentes
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(c(pacotes[!pacotes_instalados]))
}

# carregar pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


# estabelecer parâmetros da análise
caixa.disponivel <- 1000
data.investimento <- ymd("2015-12-30")

#### carregar tabelas salvas ####
precos.acoes <- readRDS("Data/retorno_volatilidade_acoes")
precos.ibov <- readRDS("Data/precosibov")
precos.acoes$price.adjusted <- round(precos.acoes$price.adjusted, 2)

#### mensalizar retornos dos ativos ####
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

#### carregar carteiras salvas ####
carteira.MT <- readRDS("Data/carteira_eficiente")
Carteiras.TOPSIS <- readRDS("Data/carteirasTOPSIS")
carteira1.TOPSIS <- Carteiras.TOPSIS$carteira1
carteira1.TOPSIS <- carteira1.TOPSIS[,c("ticker", "pesos")]
carteira2.TOPSIS <- Carteiras.TOPSIS$carteira2
carteira2.TOPSIS <- carteira2.TOPSIS[,c("ticker", "pesos")]
carteira3.TOPSIS <- Carteiras.TOPSIS$carteira3
carteira3.TOPSIS <- carteira3.TOPSIS[,c("ticker", "pesos")]

#### calcular retorno mensal do benchmark ####
retorno.mensal.ibov <- periodo.analise.ibov %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb")
retorno.mensal.ibov$ticker <- NULL

#### Performance e Visualizacao Carteira IBOVESPA ####

retorno.mensal.ibov <- periodo.analise.ibov %>%
  group_by(ticker) %>%
  tq_transmute(select = price.adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb")

crescimento.Ibov <- retorno.mensal.ibov %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Rb,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

portifo.Ibov.anualizado <- retorno.mensal.ibov %>%
  tq_performance(Ra = Rb, Rb = NULL, performance_fun = table.AnnualizedReturns,
                 Rf = 0.07644/12)

portifo.Ibov.MaxDrawdown <- retorno.mensal.ibov %>%
  tq_performance(Ra = Rb, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.Ibov.MaxDrawdown <- tibble(MDD = portifo.Ibov.MaxDrawdown$MaximumDrawdown)

portifo.Ibov.avgDrawdown <- retorno.mensal.ibov %>%
  tq_performance(Ra = Rb, Rb = NULL, performance_fun = AverageDrawdown)

portifo.Ibov.avgRecovery <- retorno.mensal.ibov %>%
  tq_performance(Ra = Rb, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.Ibov <- tibble(Portfolio = c("Ibovespa"))
resumo.portifo.Ibov$AnnualizedReturn <- mean(portifo.Ibov.anualizado$AnnualizedReturn)
resumo.portifo.Ibov$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.Ibov.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.Ibov$AnnualizedStdDev <- mean(portifo.Ibov.anualizado$AnnualizedStdDev)
resumo.portifo.Ibov$MDD <- mean(portifo.Ibov.MaxDrawdown$MDD)
resumo.portifo.Ibov$avgDD <- mean(portifo.Ibov.avgDrawdown$AverageDrawdown)
resumo.portifo.Ibov$avgRec <- mean(portifo.Ibov.avgRecovery$AverageRecovery)

retorno.mensal.ibov %>%
  ggplot(aes(x = ref.date, y = Rb)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno do Portifólio Eficiente",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.Ibov %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento Capital",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))


#### Performance e Visualizacao Carteira Moderna Teoria ####

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
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

portifo.MT.anualizado <- retorno.portifolio.MT %>%
  tq_performance(Ra = Ret.MT, Rb = NULL, performance_fun = table.AnnualizedReturns,
                 Rf = 0.07644/12)

portifo.MT.MaxDrawdown <- retorno.portifolio.MT %>%
  tq_performance(Ra = Ret.MT, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.MT.MaxDrawdown <- tibble(MDD = portifo.MT.MaxDrawdown$MaximumDrawdown)

portifo.MT.avgDrawdown <- retorno.portifolio.MT %>%
  tq_performance(Ra = Ret.MT, Rb = NULL, performance_fun = AverageDrawdown)

portifo.MT.avgRecovery <- retorno.portifolio.MT %>%
  tq_performance(Ra = Ret.MT, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.MT <- tibble(Portfolio = c("Moderna Teoria de Carteiras"))
resumo.portifo.MT$AnnualizedReturn <- mean(portifo.MT.anualizado$AnnualizedReturn)
resumo.portifo.MT$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.MT.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.MT$AnnualizedStdDev <- mean(portifo.MT.anualizado$AnnualizedStdDev)
resumo.portifo.MT$MDD <- mean(portifo.MT.MaxDrawdown$MDD)
resumo.portifo.MT$avgDD <- mean(portifo.MT.avgDrawdown$AverageDrawdown)
resumo.portifo.MT$avgRec <- mean(portifo.MT.avgRecovery$AverageRecovery)


retorno.MT <- retorno.portifolio.MT %>%
  ggplot(aes(x = ref.date, y = Ret.MT)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retornos da Carteira Eficiente",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.MT <- crescimento.portifolio.MT %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento do Capital",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

gA <- ggplotGrob(retorno.MT)
gB <- ggplotGrob(crescimento.MT)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))

#### Performance e Visualizacao Carteira TOPSIS ####

# TOPSIS - 15 primeiros ativos

precos.carteira1.TOPSIS <- left_join(carteira1.TOPSIS, select(periodo.analise,
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
               weights = carteira1.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio1.TOPSIS <- retorno.ativos.carteira1.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = carteira1.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

portifo.TOPSIS1.anualizado <- retorno.portifolio1.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.AnnualizedReturns, Rf = 0.07644/12)

portifo.TOPSIS1.MaxDrawdown <- retorno.portifolio1.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.TOPSIS1.MaxDrawdown <- tibble(MDD = portifo.TOPSIS1.MaxDrawdown$MaximumDrawdown)

portifo.TOPSIS1.avgDrawdown <- retorno.portifolio1.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageDrawdown)

portifo.TOPSIS1.avgRecovery <- retorno.portifolio1.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.TOPSIS1 <- tibble(Portfolio = c("TOPSIS 1"))
resumo.portifo.TOPSIS1$AnnualizedReturn <- mean(portifo.TOPSIS1.anualizado$AnnualizedReturn)
resumo.portifo.TOPSIS1$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.TOPSIS1.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.TOPSIS1$AnnualizedStdDev <- mean(portifo.TOPSIS1.anualizado$AnnualizedStdDev)
resumo.portifo.TOPSIS1$MDD <- mean(portifo.TOPSIS1.MaxDrawdown$MDD)
resumo.portifo.TOPSIS1$avgDD <- mean(portifo.TOPSIS1.avgDrawdown$AverageDrawdown)
resumo.portifo.TOPSIS1$avgRec <- mean(portifo.TOPSIS1.avgRecovery$AverageRecovery)

retorno.TOPSIS1 <- retorno.portifolio1.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno da Carteira TOPSIS 1",
       x = "", y = "Retorno Mensal") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.TOPSIS1 <- crescimento.portifolio1.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento do Capital",
       x = "", y = "Valor do Portifólio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

gA <- ggplotGrob(retorno.TOPSIS1)
gB <- ggplotGrob(crescimento.TOPSIS1)

# TOPSIS - 15 piores ativos

precos.carteira2.TOPSIS <- left_join(carteira2.TOPSIS, select(periodo.analise,
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
               weights = carteira2.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio2.TOPSIS <- retorno.ativos.carteira2.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = carteira2.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

portifo.TOPSIS2.anualizado <- retorno.portifolio2.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.AnnualizedReturns, Rf = 0.07644/12)

portifo.TOPSIS2.MaxDrawdown <- retorno.portifolio2.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.TOPSIS2.MaxDrawdown <- tibble(MDD = portifo.TOPSIS2.MaxDrawdown$MaximumDrawdown)

portifo.TOPSIS2.avgDrawdown <- retorno.portifolio2.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageDrawdown)

portifo.TOPSIS2.avgRecovery <- retorno.portifolio2.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.TOPSIS2 <- tibble(Portfolio = c("TOPSIS 2"))
resumo.portifo.TOPSIS2$AnnualizedReturn <- mean(portifo.TOPSIS2.anualizado$AnnualizedReturn)
resumo.portifo.TOPSIS2$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.TOPSIS2.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.TOPSIS2$AnnualizedStdDev <- mean(portifo.TOPSIS2.anualizado$AnnualizedStdDev)
resumo.portifo.TOPSIS2$MDD <- mean(portifo.TOPSIS2.MaxDrawdown$MDD)
resumo.portifo.TOPSIS2$avgDD <- mean(portifo.TOPSIS2.avgDrawdown$AverageDrawdown)
resumo.portifo.TOPSIS2$avgRec <- mean(portifo.TOPSIS2.avgRecovery$AverageRecovery)

retorno.TOPSIS2 <- retorno.portifolio2.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno da Carteira TOPSIS 2",
       x = "", y = "") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.TOPSIS2 <- crescimento.portifolio2.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento do Capital",
       x = "", y = "") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

gC <- ggplotGrob(retorno.TOPSIS2)
gD <- ggplotGrob(crescimento.TOPSIS2)



# TOPSIS - 1 ativo de cada setor - melhor pontuação

precos.carteira3.TOPSIS <- left_join(carteira3.TOPSIS, select(periodo.analise,
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
               weights = carteira3.TOPSIS,
               col_rename = "Ret.TOPSIS")

crescimento.portifolio3.TOPSIS <- retorno.ativos.carteira3.TOPSIS %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = carteira3.TOPSIS,
               col_rename   = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

portifo.TOPSIS3.anualizado <- retorno.portifolio3.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.AnnualizedReturns, Rf = 0.07644/12)

portifo.TOPSIS3.MaxDrawdown <- retorno.portifolio3.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.TOPSIS3.MaxDrawdown <- tibble(MDD = portifo.TOPSIS3.MaxDrawdown$MaximumDrawdown)

portifo.TOPSIS3.avgDrawdown <- retorno.portifolio3.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageDrawdown)

portifo.TOPSIS3.avgRecovery <- retorno.portifolio3.TOPSIS %>%
  tq_performance(Ra = Ret.TOPSIS, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.TOPSIS3 <- tibble(Portfolio = c("TOPSIS 3"))
resumo.portifo.TOPSIS3$AnnualizedReturn <- mean(portifo.TOPSIS3.anualizado$AnnualizedReturn)
resumo.portifo.TOPSIS3$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.TOPSIS3.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.TOPSIS3$AnnualizedStdDev <- mean(portifo.TOPSIS3.anualizado$AnnualizedStdDev)
resumo.portifo.TOPSIS3$MDD <- mean(portifo.TOPSIS3.MaxDrawdown$MDD)
resumo.portifo.TOPSIS3$avgDD <- mean(portifo.TOPSIS3.avgDrawdown$AverageDrawdown)
resumo.portifo.TOPSIS3$avgRec <- mean(portifo.TOPSIS3.avgRecovery$AverageRecovery)

retorno.TOPSIS3 <- retorno.portifolio3.TOPSIS %>%
  ggplot(aes(x = ref.date, y = Ret.TOPSIS)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Retorno da Carteira TOPSIS 3",
       x = "", y = "") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

crescimento.TOPSIS3 <- crescimento.portifolio3.TOPSIS %>%
  ggplot(aes(x = ref.date, y = crescimento.capital)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Crescimento do Capital",
       x = "", y = "") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))

gE <- ggplotGrob(retorno.TOPSIS3)
gF <- ggplotGrob(crescimento.TOPSIS3)
grid::grid.newpage()
grid::grid.draw(cbind(rbind(gA, gB),
                      rbind(gC, gD),
                      rbind(gE, gF)))

#### Performance Carteira Aleatória ####

n = 1000

carteira.aleatoria <- list()

prob.MGLU3 <- ifelse(unique(precos.acoes$ticker) == "MGLU3", 0.10, 0.90)


for(i in 1:n){
  carteira.aleatoria[[i]] <- tibble("portifolio" = i, "ticker" = sample(unique(precos.acoes$ticker),
                                                             size = sample(c(8, 9, 10, 11, 12, 13, 14, 15),
                                                                           size = 1),
                                                             replace = F,
                                                             prob = prob.MGLU3))
  carteira.aleatoria[[i]]$pesos.aux <- sample(c(10, 20, 30, 40, 50),
                                                     size = nrow(carteira.aleatoria[[i]]),
                                                     replace = T)
  carteira.aleatoria[[i]]$pesos <- carteira.aleatoria[[i]]$pesos.aux/sum(carteira.aleatoria[[i]]$pesos.aux)

  carteira.aleatoria[[i]]$pesos.aux <- NULL

  carteira.aleatoria[[i]] <- full_join(x = carteira.aleatoria[[i]],
                                       y = unique(select(periodo.analise,
                                                         ticker)))
  carteira.aleatoria[[i]]$portifolio <- replace_na(carteira.aleatoria[[i]]$portifolio,
                                                   as.integer(i))
  carteira.aleatoria[[i]]$pesos <- replace_na(carteira.aleatoria[[i]]$pesos, 0)
  carteira.aleatoria[[i]] <- carteira.aleatoria[[i]] %>%
    arrange(ticker)

}

carteira.aleatoria <- do.call(rbind, carteira.aleatoria)

pesos <- c(carteira.aleatoria$pesos)

ticker <- precos.acoes %>%
  arrange(ticker) %>%
  select(ticker) %>%
  unique(.)
ticker <- c(ticker$ticker)

tabela.pesos <- tibble(ticker) %>%
  tq_repeat_df(n = n) %>%
  bind_cols(tibble(pesos)) %>%
  group_by(portfolio)


retorno.ativos.mensal.multi <- retorno.mensal.ativos %>%
  subset(ref.date>data.investimento) %>%
  tq_repeat_df(n=n)

retorno.mensal.portifolio.multi <- retorno.ativos.mensal.multi %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = tabela.pesos,
               col_rename = "Ra")

crescimento.mensal.portifolio.multi <- retorno.ativos.mensal.multi %>%
  tq_portfolio(assets_col = ticker,
               returns_col = Ra,
               weights = tabela.pesos,
               col_rename = "crescimento.capital",
               wealth.index = TRUE) %>%
  mutate(crescimento.capital = crescimento.capital * caixa.disponivel)

RaRb_portifolio.multi <- left_join(retorno.mensal.portifolio.multi,
                                   retorno.mensal.ibov, by = "ref.date")

portifo.aleatorio.anualizado <- RaRb_portifolio.multi %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns,
                 Rf = 0.07644/12)

portifo.aleatorio.MaxDrawdown <- RaRb_portifolio.multi %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk)
portifo.aleatorio.MaxDrawdown <- tibble(MDD = portifo.aleatorio.MaxDrawdown$MaximumDrawdown)

portifo.aleatorio.avgDrawdown <- RaRb_portifolio.multi %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = AverageDrawdown)

portifo.aleatorio.avgRecovery <- RaRb_portifolio.multi %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = AverageRecovery)

resumo.portifo.aleatorio <- tibble(Portfolio = c("Aleatorio"))
resumo.portifo.aleatorio$AnnualizedReturn <- mean(portifo.aleatorio.anualizado$AnnualizedReturn)
resumo.portifo.aleatorio$`AnnualizedSharpe(Rf=7.64%)` <- mean(portifo.aleatorio.anualizado$`AnnualizedSharpe(Rf=7.64%)`)
resumo.portifo.aleatorio$AnnualizedStdDev <- mean(portifo.aleatorio.anualizado$AnnualizedStdDev)
resumo.portifo.aleatorio$MDD <- mean(portifo.aleatorio.MaxDrawdown$MDD)
resumo.portifo.aleatorio$avgDD <- mean(portifo.aleatorio.avgDrawdown$AverageDrawdown)
resumo.portifo.aleatorio$avgRec <- mean(portifo.aleatorio.avgRecovery$AverageRecovery)

table(carteira.aleatoria$ticker == "MGLU3")

crescimento.mensal.portifolio.multi%>%
  ggplot(aes(x = ref.date, y = crescimento.capital, color = factor(portfolio))) +
  geom_line(size = 2) +
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       x = "", y = "Portfolio Value",
       color = "Portfolio") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)




###### teste carteira 3 #######

# acao <- tibble("ticker" = c("MGLU3"))
#
# periodo.analise.acao <- subset(periodo.analise, periodo.analise$ticker == acao$ticker)
#
# precos.carteira.acao <- left_join(acao, select(periodo.analise.acao, ticker,
#                                                     ref.date, price.adjusted),
#                                 by = "ticker")
#
#
# retorno.ativos.carteira.acao <- precos.carteira.acao %>%
#   group_by(ticker) %>%
#   tq_transmute(select = price.adjusted,
#                mutate_fun = periodReturn,
#                period = "monthly",
#                col_rename = "Ra")
#
# retorno.portifolio.acao <- retorno.ativos.carteira.acao %>%
#   tq_portfolio(assets_col = ticker,
#                returns_col = Ra,
#                col_rename = "Ret.Acao")
#
# crescimento.portifolio.acao <- retorno.ativos.carteira.acao %>%
#   tq_portfolio(assets_col = ticker,
#                returns_col = Ra,
#                col_rename   = "crescimento.capital",
#                wealth.index = TRUE) %>%
#   mutate(crescimento.capital = crescimento.capital * caixa.disponivel)
#
# retorno.portifolio.acao %>%
#   ggplot(aes(x = ref.date, y = Ret.Acao)) +
#   geom_bar(stat = "identity", fill = palette_light()[[1]]) +
#   labs(title = "Retorno do Ativo",
#        x = "", y = "Retorno Mensal") +
#   geom_smooth(method = "lm") +
#   theme_tq() +
#   scale_color_tq() +
#   scale_y_continuous(labels = scales::percent)
#
# crescimento.portifolio.acao %>%
#   ggplot(aes(x = ref.date, y = crescimento.capital)) +
#   geom_line(size = 2, color = palette_light()[[1]]) +
#   labs(title = "Crescimento Capital",
#        x = "", y = "Valor do Ativo") +
#   geom_smooth(method = "loess") +
#   theme_tq() +
#   scale_color_tq() +
#   scale_y_continuous(labels = scales::label_dollar(prefix = "R$ "))


#### construir a tabela de resumo das performances e visualizar em gráfico ####
resumo <- resumo.portifo.MT %>%
  rbind(resumo.portifo.TOPSIS1) %>%
  rbind(resumo.portifo.TOPSIS2) %>%
  rbind(resumo.portifo.TOPSIS3) %>%
  rbind(resumo.portifo.Ibov) %>%
  rbind(resumo.portifo.aleatorio)

resumo %>%
  ggplot(aes(x = AnnualizedStdDev, y = AnnualizedReturn,
             color = factor(Portfolio))) +
  geom_point(size = 4) +
  labs(title = "Comparação do Risco x Retorno entre os Portifólios",
       subtitle = "Período de 2016 a 2020",
       x = "Risco a. a.", y = "Retorno a. a.",
       color = "Portfolio") +
  theme_tq() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(labels = scales::label_percent())


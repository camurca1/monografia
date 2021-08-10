# Escrito por: Alexandre Camurça Silva de Souza
# Ambiente RStudio Desktop 1.4.1717 "Juliet Rose"

# Etapa 7 - Cálculo da fronteira e da carteira eficiente


# limpar memória e desativar notação científica
rm(list = ls())
options(scipen = 999)

#### Gereciamento de pacotes ####

# informar os pacotes que serao utilizados no script
pacotes <- c("tidyverse", "fPortfolio")

# instalar pacotes ausentes
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(pacotes_instalados == FALSE)) {
  install.packages(c(pacotes[!pacotes_instalados]))
}

# carregar pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


#### carregar tabelas salvas ####
ret.vol.acoes <- readRDS("Data/retorno_volatilidade_acoes")

precos.acoes <- as.data.frame(ret.vol.acoes$CD_CVM)
precos.acoes$ticker <- ret.vol.acoes$ticker
precos.acoes$ref.date <- ret.vol.acoes$ref.date
precos.acoes$retornos <- ret.vol.acoes$retornos
precos.acoes <- subset(precos.acoes, precos.acoes$ref.date < "2016-01-01")

#### construir a matriz de retornos ####
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


#### definir parâmetros do cálculo da fronteira ####
specs <- portfolioSpec()
`setNFrontierPoints<-`(specs, 1000)


#### calcular fronteira eficiente e gerar gráficos ####
portfolio.eficiente <- tangencyPortfolio(retornos.matriz, spec = specs,
                                         constraints = "LongOnly")
weightsPie(portfolio.eficiente, col = qualiPalette(n = 76, "Set1"))
mtext(text = "Pesos - Portifolio Eficiente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)


fronteira <- portfolioFrontier(retornos.matriz, spec = specs,
                               constraints = "LongOnly")


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
                  text(xy[, 1], xy[, 2], pos = 3,
                       rownames(xy), font = 2, cex = 0.7))

dados.portifolio <- getPortfolio(portfolio.eficiente)

#### extrair carteira eficiente ####
carteira <- as.data.frame(dados.portifolio$weights)
carteira$Tickers <- row.names(carteira)
row.names(carteira) <- NULL
carteira <- subset(carteira, carteira$`dados.portifolio$weights` > 0)
names(carteira)[1] <- "Pesos"
carteira <- carteira %>% select(Tickers, Pesos)

saveRDS(dados.portifolio, "Data/dados_carteira_eficiente")
saveRDS(carteira, "Data/carteira_eficiente")
write.csv2(carteira, "Data/carteira_eficiente.csv", row.names = F)

# limpeza de memória
rm(list = ls())
gc()

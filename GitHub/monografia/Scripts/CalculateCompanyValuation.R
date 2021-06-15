rm(list = ls())
options(scipen = 999)


library(tidyverse)
library(lubridate)
library(bizdays)
library(stringr)


holidaysANBIMA <- append(holidaysANBIMA,  ymd(c("2016-12-30", "2017-12-29")))
cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))


dfp.empresas <- readRDS("Data/cia_dfp")
cia.info <- readRDS("Data/cia_info_reduzido")
precos.empresas <- readRDS("Data/precos_acoes")
tickers.negociados <- readRDS("Data/tickers_mais_negociados")
fre.empresas <- readRDS("Data/cia_fre")
volatilidade.empresas <- readRDS("Data/retorno_volatilidade_acoes")

precos.empresas$vol.financeiro.dia <- precos.empresas$price.close*precos.empresas$volume

names(precos.empresas)[7] <- "DT_REFER"
names(precos.empresas)[8] <- "simbolo"

media.financeira <- precos.empresas %>%
  filter(year(DT_REFER) > 2014) %>%
  group_by(simbolo, ano = year(DT_REFER)) %>%
  summarize(., volume.medio.financeiro.ano = round(mean(vol.financeiro.dia), 0))
media.financeira <- media.financeira %>%
  filter(volume.medio.financeiro.ano > 2000000) %>%
  mutate(DT_REFER = ymd(paste0(ano, "-12-31")))
media.financeira$ano <- NULL

media.volatilidade <- volatilidade.empresas %>%
  filter(year(ref.date) > 2014) %>%
  group_by(ticker, ano = year(ref.date)) %>%
  summarize(., volatilidade.media.ano = mean(volatilidade)) %>%
  mutate(DT_REFER = ymd(paste0(ano, "-12-31")))
media.volatilidade$ano <- NULL
names(media.volatilidade)[1] <- "simbolo"

ativo.empresas <- dfp.empresas$`DF Consolidado - Balanço Patrimonial Ativo`
passivo.empresas <- dfp.empresas$`DF Consolidado - Balanço Patrimonial Passivo`
dre.empresas <- dfp.empresas$`DF Consolidado - Demonstração do Resultado`
dfc.empresas <- dfp.empresas$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
capital.empresas <- fre.empresas$df_capital
capital.empresas <- capital.empresas %>%
  group_by(CD_CVM, DT_REFER) %>%
  summarise(total.acoes = sum(qtd.issued))
capital.empresas$DT_REFER <- ymd(capital.empresas$DT_REFER)-1
dividend.yield <- fre.empresas$df_dividends_details
dividend.yield$DT_REFER <- (dividend.yield$DT_REFER)-1


ativo.empresas <- left_join(ativo.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   listing.segment,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
ativo.empresas <- subset(ativo.empresas,
                         ativo.empresas$sub.sector != "Intermediários Financeiros")

passivo.empresas <- left_join(passivo.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
passivo.empresas <- subset(passivo.empresas,
                           passivo.empresas$sub.sector != "Intermediários Financeiros")

dre.empresas <- left_join(dre.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
dre.empresas <- subset(dre.empresas,
                       dre.empresas$sub.sector != "Intermediários Financeiros")
dfc.empresas <- left_join(dfc.empresas,
                          select(cia.info,
                                 codigos.cvm,
                                 sub.sector,
                                 segment),
                          by = c("CD_CVM" = "codigos.cvm"))
dfc.empresas <- subset(dfc.empresas,
                       dfc.empresas$sub.sector != "Intermediários Financeiros")


ativo.empresas$DT_REFER <- ymd(ativo.empresas$DT_REFER)
passivo.empresas$DT_REFER <- ymd(passivo.empresas$DT_REFER)
dre.empresas$DT_REFER <- ymd(dre.empresas$DT_REFER)
dfc.empresas$DT_REFER <- ymd(dfc.empresas$DT_REFER)
precos.empresas$DT_REFER <- ymd(precos.empresas$DT_REFER)

ativoCirculante <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1.01')
ativoRLP <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1.02.01')
ativoTotal <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1')

passivoELP <- subset(passivo.empresas, passivo.empresas$CD_CONTA == '2.02')
passivoCirculante <- subset(passivo.empresas, passivo.empresas$CD_CONTA == '2.01')

dreReceitaLiq <- subset(dre.empresas, dre.empresas$CD_CONTA == '3.01')
dreEBIT <- subset(dre.empresas,
                  dre.empresas$DS_CONTA == "Resultado Antes do Resultado Financeiro e dos Tributos")
dreLucroLiq <- subset(dre.empresas, dre.empresas$CD_CONTA == '3.11')
dreLucroLiq <- subset(dreLucroLiq, dreLucroLiq$CD_CVM != 3115)
dreLucroLiq <- subset(dreLucroLiq, dreLucroLiq$CD_CVM != 23159)
dreLucroLiq <- rbind(dreLucroLiq, filter(dre.empresas,
                                         dre.empresas$CD_CVM == 3115,
                                         dre.empresas$CD_CONTA == '3.13'))
dreLucroLiq <- rbind(dreLucroLiq, filter(dre.empresas,
                                         dre.empresas$CD_CVM == 23159,
                                         dre.empresas$CD_CONTA == '3.13'))
DeprecAmort <- dfc.empresas[agrep(pattern="depreciação",
                                  dfc.empresas$DS_CONTA,
                                   ignore.case = TRUE), ]
DeprecAmort <- DeprecAmort[agrep(pattern="amortização",
                                 DeprecAmort$DS_CONTA,
                                   ignore.case = TRUE), ]
DeprecAmort <- subset(DeprecAmort, DeprecAmort$VL_CONTA != 0)
DeprecAmort <- subset(DeprecAmort, DeprecAmort$CD_CVM != 22799)
DeprecAmort <- rbind(DeprecAmort, filter(dfc.empresas,
                                         dfc.empresas$CD_CVM == 22799,
                                         dfc.empresas$CD_CONTA == '6.01.01.02'))

rm(ativo.empresas, passivo.empresas, dfc.empresas, volatilidade.empresas,
   dre.empresas, dfp.empresas, cia.info, fre.empresas)
gc()

bp <- distinct(full_join(ativoCirculante,
                         select(ativoRLP,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_ATIVO_RLP = ESCALA_MOEDA,
                                COD_ATIVO_RLP = CD_CONTA,
                                NOME_ATIVO_RLP = DS_CONTA,
                                VALOR_ATIVO_RLP = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(full_join(bp,
                         select(ativoTotal,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_ATIVO_TOTAL = ESCALA_MOEDA,
                                COD_ATIVO_TOTAL = CD_CONTA,
                                NOME_ATIVO_TOTAL = DS_CONTA,
                                VALOR_ATIVO_TOTAL = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(full_join(bp,
                         select(passivoCirculante,
                                    CD_CVM,
                                    DT_REFER,
                                    ESCALA_MOEDA_PASSIVO_CIRCULANTE = ESCALA_MOEDA,
                                    COD_PASSIVO_CIRCULANTE = CD_CONTA,
                                    NOME_PASSIVO_CIRCULANTE = DS_CONTA,
                                    VALOR_PASSIVO_CIRCULANTE = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(full_join(bp,
                         select(passivoELP,
                                    CD_CVM,
                                    DT_REFER,
                                    ESCALA_MOEDA_PASSIVO_ELP = ESCALA_MOEDA,
                                    COD_PASSIVO_ELP = CD_CONTA,
                                    NOME_PASSIVO_ELP = DS_CONTA,
                                    VALOR_PASSIVO_ELP = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(left_join(bp,
                         select(dreReceitaLiq,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_DRE_RL = ESCALA_MOEDA,
                                COD_DRE_RL = CD_CONTA,
                                NOME_DRE_RL = DS_CONTA,
                                VALOR_DRE_RL = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(left_join(bp,
                         select(dreLucroLiq,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_DRE_LL = ESCALA_MOEDA,
                                COD_DRE_LL = CD_CONTA,
                                NOME_DRE_LL = DS_CONTA,
                                VALOR_DRE_LL = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(left_join(bp,
                         select(dreEBIT,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_DRE_EBIT = ESCALA_MOEDA,
                                COD_DRE_EBIT = CD_CONTA,
                                NOME_DRE_EBIT = DS_CONTA,
                                VALOR_DRE_EBIT = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp <- distinct(left_join(bp,
                         select(DeprecAmort,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_DFC_DA = ESCALA_MOEDA,
                                COD_DFC_DA = CD_CONTA,
                                NOME_DFC_DA = DS_CONTA,
                                VALOR_DFC_DA = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))
bp$EBITDA <- bp$VALOR_DRE_EBIT + bp$VALOR_DFC_DA
bp <- left_join(bp, select(tickers.negociados,
                           CD_CVM,
                           simbolo),
                by="CD_CVM")
bp <- filter(bp, !is.na(bp$simbolo))
bp$dia.util.anterior <- add.bizdays(bp$DT_REFER, -1, cal)
bp <- left_join(bp,
                select(precos.empresas,
                       simbolo,
                       DT_REFER,
                       price.close,
                       volume),
                by = c("simbolo", "dia.util.anterior" = "DT_REFER"))
bp <- left_join(bp, select(capital.empresas,
                           CD_CVM,
                           DT_REFER,
                           total.acoes),
                by=c("CD_CVM", "DT_REFER"))
bp <- left_join(bp, select(dividend.yield,
                           CD_CVM,
                           DT_REFER,
                           distributed.dividend),
                by=c("CD_CVM", "DT_REFER"))

rm(ativoCirculante, ativoRLP, ativoTotal,
   passivoCirculante, passivoELP, dreLucroLiq,
   precos.empresas, tickers.negociados, capital.empresas,
   dividend.yield, DeprecAmort, dreEBIT, dreReceitaLiq)
gc()

bp$VL_CONTA <- if_else(bp$ESCALA_MOEDA == "MIL",
                       bp$VL_CONTA*1000,
                       bp$VL_CONTA)
bp$ESCALA_MOEDA <- NULL
bp$VALOR_ATIVO_RLP <- if_else(bp$ESCALA_MOEDA_ATIVO_RLP == "MIL",
                              bp$VALOR_ATIVO_RLP*1000,
                              bp$VALOR_ATIVO_RLP)
bp$ESCALA_MOEDA_ATIVO_RLP <- NULL
bp$VALOR_ATIVO_TOTAL <- if_else(bp$ESCALA_MOEDA_ATIVO_TOTAL == "MIL",
                                bp$VALOR_ATIVO_TOTAL*1000,
                                bp$VALOR_ATIVO_TOTAL)
bp$ESCALA_MOEDA_ATIVO_TOTAL <- NULL
bp$VALOR_PASSIVO_CIRCULANTE <- if_else(bp$ESCALA_MOEDA_PASSIVO_CIRCULANTE == "MIL",
                                       bp$VALOR_PASSIVO_CIRCULANTE*1000,
                                       bp$VALOR_PASSIVO_CIRCULANTE)
bp$ESCALA_MOEDA_PASSIVO_CIRCULANTE <- NULL
bp$VALOR_PASSIVO_ELP <- if_else(bp$ESCALA_MOEDA_PASSIVO_ELP == "MIL",
                                bp$VALOR_PASSIVO_ELP*1000,
                                bp$VALOR_PASSIVO_ELP)
bp$ESCALA_MOEDA_PASSIVO_ELP <- NULL
bp$VALOR_DRE_RL <- if_else(bp$ESCALA_MOEDA_DRE_RL == "MIL",
                             bp$VALOR_DRE_RL*1000,
                             bp$VALOR_DRE_RL)
bp$ESCALA_MOEDA_DRE_RL <- NULL
bp$VALOR_DRE_EBIT <- if_else(bp$ESCALA_MOEDA_DRE_EBIT == "MIL",
                           bp$VALOR_DRE_EBIT*1000,
                           bp$VALOR_DRE_EBIT)
bp$ESCALA_MOEDA_DRE_EBIT <- NULL
bp$VALOR_DRE_LL <- if_else(bp$ESCALA_MOEDA_DRE_LL == "MIL",
                           bp$VALOR_DRE_LL*1000,
                           bp$VALOR_DRE_LL)
bp$ESCALA_MOEDA_DRE_LL <- NULL
bp$VALOR_DFC_DA <- if_else(bp$ESCALA_MOEDA_DFC_DA == "MIL",
                           bp$VALOR_DFC_DA*1000,
                           bp$VALOR_DFC_DA)
bp$VALOR_DFC_DA <- replace(bp$VALOR_DFC_DA, is.na(bp$VALOR_DFC_DA), 0)
bp$ESCALA_MOEDA_DFC_DA <- NULL
bp$source_file <- NULL
bp$COLUNA_DF <- NULL
bp$DT_INI_EXERC <- NULL
bp$dia.util.anterior <- NULL

volume.ano <- as.data.frame(bp$CD_CVM)
volume.ano$DT_REFER <- bp$DT_REFER
volume.ano$simbolo <- bp$simbolo
names(volume.ano)[1:3] <- c("CD_CVM", "DT_REFER", "simbolo")
volume.ano <-  left_join(volume.ano,
                         media.financeira,
                         c("simbolo", "DT_REFER" = "DT_REFER"))
bp <- left_join(bp, volume.ano, by=c("CD_CVM", "DT_REFER", "simbolo"))
bp <- subset(bp, !is.na(bp$volume.medio.financeiro.ano))

volatilidade.ano <- as.data.frame(bp$CD_CVM)
volatilidade.ano$DT_REFER <- bp$DT_REFER
volatilidade.ano$simbolo <- bp$simbolo
names(volatilidade.ano)[1:3] <- c("CD_CVM", "DT_REFER", "simbolo")
volatilidade.ano <- left_join(volatilidade.ano,
                              media.volatilidade,
                              c("DT_REFER", "simbolo"))
bp <- left_join(bp, volatilidade.ano, by=c("CD_CVM", "DT_REFER", "simbolo"))

obs.completas <- bp %>%
  count(CD_CVM) %>%
  filter(n==6)

bp <- left_join(obs.completas, bp, by= "CD_CVM")
names(bp)[1] <- "CD_CVM"
bp$n <- NULL


rm(obs.completas, media.financeira, volume.ano, media.volatilidade, volatilidade.ano)
gc()

saveRDS(bp, "Data/balanco_patrimonial")

indices <- as.data.frame(bp$CD_CVM)
names(indices)[1] <- "CD_CVM"
indices$ticker <- bp$simbolo
indices$DT_REFER <- bp$DT_REFER
indices$GC <- bp$listing.segment
indices$Volatilidade <- bp$volatilidade.media.ano
indices$LG <- (bp$VL_CONTA+bp$VALOR_ATIVO_RLP)/(bp$VALOR_PASSIVO_CIRCULANTE+bp$VALOR_PASSIVO_ELP)
indices$ROA <- (bp$VALOR_DRE_LL/bp$VALOR_ATIVO_TOTAL)
indices$IPL <- (bp$price.close/(bp$VALOR_DRE_LL/bp$total.acoes))
indices$MEBITIDA <- (bp$EBITDA/bp$VALOR_DRE_RL)
saveRDS(indices, "Data/indices_calculados")


rm(list = ls())
gc()



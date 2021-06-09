rm(list = ls())
options(scipen = 999)


library(tidyverse)
library(lubridate)

dfp.empresas <- readRDS("Data/cia_dfp")
cia.info <- readRDS("Data/cia_info_reduzido")

ativo.empresas <- dfp.empresas$`DF Consolidado - Balanço Patrimonial Ativo`
passivo.empresas <- dfp.empresas$`DF Consolidado - Balanço Patrimonial Passivo`
dre.empresas <- dfp.empresas$`DF Consolidado - Demonstração do Resultado`

ativo.empresas <- left_join(ativo.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
ativo.empresas <- subset(ativo.empresas, ativo.empresas$sub.sector != "Intermediários Financeiros")

passivo.empresas <- left_join(passivo.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
passivo.empresas <- subset(passivo.empresas, passivo.empresas$sub.sector != "Intermediários Financeiros")

dre.empresas <- left_join(dre.empresas,
                            select(cia.info,
                                   codigos.cvm,
                                   sub.sector,
                                   segment),
                            by = c("CD_CVM" = "codigos.cvm"))
dre.empresas <- subset(dre.empresas, dre.empresas$sub.sector != "Intermediários Financeiros")

ativo.empresas$DT_REFER <- ymd(ativo.empresas$DT_REFER)
passivo.empresas$DT_REFER <- ymd(passivo.empresas$DT_REFER)
dre.empresas$DT_REFER <- ymd(dre.empresas$DT_REFER)

ativoCirculante <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1.01')
ativoRLP <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1.02.01')
ativoTotal <- subset(ativo.empresas, ativo.empresas$CD_CONTA == '1')

passivoELP <- subset(passivo.empresas, passivo.empresas$CD_CONTA == '2.02')
passivoCirculante <- subset(passivo.empresas, passivo.empresas$CD_CONTA == '2.01')

dreLucroLiq <- subset(dre.empresas, dre.empresas$CD_CONTA == '3.11')
dreLucroLiq <- subset(dreLucroLiq, dreLucroLiq$CD_CVM != 3115)
dreLucroLiq <- subset(dreLucroLiq, dreLucroLiq$CD_CVM != 23159)
dreLucroLiq <- rbind(dreLucroLiq, filter(dre.empresas,
                                         dre.empresas$CD_CVM == 3115,
                                         dre.empresas$CD_CONTA == '3.13'))
dreLucroLiq <- rbind(dreLucroLiq, filter(dre.empresas,
                                         dre.empresas$CD_CVM == 23159,
                                         dre.empresas$CD_CONTA == '3.13'))

rm(ativo.empresas, passivo.empresas, dre.empresas, dfp.empresas, cia.info)
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
bp <- distinct(full_join(bp,
                         select(dreLucroLiq,
                                CD_CVM,
                                DT_REFER,
                                ESCALA_MOEDA_DRE_LL = ESCALA_MOEDA,
                                COD_DRE_LL = CD_CONTA,
                                NOME_DRE_LL = DS_CONTA,
                                VALOR_DRE_LL = VL_CONTA),
                         by=c("CD_CVM","DT_REFER")))


rm(ativoCirculante, ativoRLP, ativoTotal, passivoCirculante, passivoELP, dreLucroLiq)
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
bp$VALOR_DRE_LL <- if_else(bp$ESCALA_MOEDA_DRE_LL == "MIL",
                           bp$VALOR_DRE_LL*1000,
                           bp$VALOR_DRE_LL)
bp$ESCALA_MOEDA_DRE_LL <- NULL
bp$source_file <- NULL
bp$COLUNA_DF <- NULL
bp$DT_INI_EXERC <- NULL
saveRDS(bp, "Data/balanco_patrimonial")

indices <- as.data.frame(bp$CD_CVM)
indices$DT_REFER <- bp$DT_REFER
indices$LG <- (bp$VL_CONTA+bp$VALOR_ATIVO_RLP)/(bp$VALOR_PASSIVO_CIRCULANTE+bp$VALOR_PASSIVO_ELP)
indices$ROA <- (bp$VALOR_DRE_LL/bp$VALOR_ATIVO_TOTAL)
saveRDS(indices, "Data/indices_calculados")

rm(ls())
gc()




#
# ativoTotal$ativo_lag <- with(ativoTotal, ave(as.numeric(ativoTotal$VL_CONTA), ativoTotal$CD_CVM, FUN = lag, -1))
#
# AtivoMedio <- ativoTotal %>%
#   group_by(CD_CVM, DT_REFER) %>%
#   summarize(ativo_medio = (as.numeric(VL_CONTA)+as.numeric(ativo_lag))/2) %>%
#   mutate(ativo_lag = NULL)
#
# ativoContasReceber$receber_lag <- with(ativoContasReceber, ave(as.numeric(ativoContasReceber$VL_CONTA), ativoContasReceber$CD_CVM, FUN = lag, -1))
#
# ReceberMedio <- ativoContasReceber %>%
#   group_by(CD_CVM, DT_REFER) %>%
#   summarize(receber_medio = (as.numeric(VL_CONTA)+as.numeric(receber_lag))/2) %>%
#   mutate(receber_lag = NULL)
#
# ativoEstoque$estoque_lag <- with(ativoEstoque, ave(as.numeric(ativoEstoque$VL_CONTA), ativoEstoque$CD_CVM, FUN = lag, -1))
#
# EstoqueMedio <- ativoEstoque %>%
#   group_by(CD_CVM, DT_REFER) %>%
#   summarize(estoque_medio = (as.numeric(VL_CONTA)+as.numeric(estoque_lag))/2) %>%
#   mutate(estoque_lag = NULL)
#
# passivoFornecedores$fornecedores_lag = with(passivoFornecedores, ave(as.numeric(passivoFornecedores$VL_CONTA), passivoFornecedores$CD_CVM, FUN = lag, -1))
#
# FornecedoresMedio <- passivoFornecedores %>%
#   group_by(CD_CVM, DT_REFER) %>%
#   summarize(fornecedores_medio = (as.numeric(VL_CONTA)+as.numeric(fornecedores_lag))/2) %>%
#   mutate(fornecedores_lag = NULL)
#
# passivoPL$PL_lag = with(passivoPL, ave(as.numeric(passivoPL$VL_CONTA), passivoPL$CD_CVM, FUN = lag, -1))
#
# PLMedio <- passivoPL %>%
#   group_by(CD_CVM, DT_REFER) %>%
#   summarize(PL_medio = (as.numeric(VL_CONTA)+as.numeric(PL_lag))/2) %>%
#   mutate(PL_lag = NULL)
#
#
# teste.df <- distinct(full_join(AtivoMedio, PLMedio))
# teste.df <- distinct(full_join(teste.df, EstoqueMedio, by = c("CD_CVM", "DT_REFER")))
# teste.df <- distinct(full_join(teste.df, ReceberMedio, by = c("CD_CVM", "DT_REFER")))
# teste.df <- distinct(full_join(teste.df, FornecedoresMedio, by = c("CD_CVM", "DT_REFER")))
# teste.df <- distinct(full_join(teste.df, select(ativoTotal, CD_CVM, DT_REFER, DENOM_CIA,
#                                                 ESCALA_MOEDA_ATIVO_TOTAL = ESCALA_MOEDA,
#                                                 COD_ATIVO_TOTAL = CD_CONTA,
#                                                 NOME_ATIVO_TOTAL = DS_CONTA,
#                                                 VALOR_ATIVO_TOTAL = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoCirculante, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_CIRCULANTE = ESCALA_MOEDA,
#                                                 COD_ATIVO_CIRCULANTE = CD_CONTA,
#                                                 NOME_ATIVO_CIRCULANTE = DS_CONTA,
#                                                 VALOR_ATIVO_CIRCULANTE = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoCaixa, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_CAIXA = ESCALA_MOEDA,
#                                                 COD_ATIVO_CAIXA = CD_CONTA,
#                                                 NOME_ATIVO_CAIXA = DS_CONTA,
#                                                 VALOR_ATIVO_CAIXA = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoContasReceber, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_CONTASRECEBER = ESCALA_MOEDA,
#                                                 COD_ATIVO_CONTASRECEBER = CD_CONTA,
#                                                 NOME_ATIVO_CONTASRECEBER = DS_CONTA,
#                                                 VALOR_ATIVO_CONTASRECEBER = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoEstoque, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_ESTOQUE = ESCALA_MOEDA,
#                                                 COD_ATIVO_ESTOQUE = CD_CONTA,
#                                                 NOME_ATIVO_ESTOQUE = DS_CONTA,
#                                                 VALOR_ATIVO_ESTOQUE = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoRLP, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_RLP = ESCALA_MOEDA,
#                                                 COD_ATIVO_RLP = CD_CONTA,
#                                                 NOME_ATIVO_RLP = DS_CONTA,
#                                                 VALOR_ATIVO_RLP = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(ativoImob, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_ATIVO_IMOB = ESCALA_MOEDA,
#                                                 COD_ATIVO_IMOB = CD_CONTA,
#                                                 NOME_ATIVO_IMOB = DS_CONTA,
#                                                 VALOR_ATIVO_IMOB = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoTotal, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_TOTAL = ESCALA_MOEDA,
#                                                 COD_PASSIVO_TOTAL = CD_CONTA,
#                                                 NOME_PASSIVO_TOTAL = DS_CONTA,
#                                                 VALOR_PASSIVO_TOTAL = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoCirculante, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_CIRCULANTE = ESCALA_MOEDA,
#                                                 COD_PASSIVO_CIRCULANTE = CD_CONTA,
#                                                 NOME_PASSIVO_CIRCULANTE = DS_CONTA,
#                                                 VALOR_PASSIVO_CIRCULANTE = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoFinCP, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_FINCP = ESCALA_MOEDA,
#                                                 COD_PASSIVO_FINCP = CD_CONTA,
#                                                 NOME_PASSIVO_FINCP = DS_CONTA,
#                                                 VALOR_PASSIVO_FINCP = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoFornecedores, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_FORNECEDORES = ESCALA_MOEDA,
#                                                 COD_PASSIVO_FORNECEDORES = CD_CONTA,
#                                                 NOME_PASSIVO_FORNECEDORES = DS_CONTA,
#                                                 VALOR_PASSIVO_FORNECEDORES = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoFinLP, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_FINLP = ESCALA_MOEDA,
#                                                 COD_PASSIVO_FINLP = CD_CONTA,
#                                                 NOME_PASSIVO_FINLP = DS_CONTA,
#                                                 VALOR_PASSIVO_FINLP = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(passivoELP, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_ELP = ESCALA_MOEDA,
#                                                 COD_PASSIVO_ELP = CD_CONTA,
#                                                 NOME_PASSIVO_ELP = DS_CONTA,
#                                                 VALOR_PASSIVO_ELP = VL_CONTA)))
# teste.df <- distinct(left_join(teste.df, select(passivoPL, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_PASSIVO_PL = ESCALA_MOEDA,
#                                                 COD_PASSIVO_PL = CD_CONTA,
#                                                 NOME_PASSIVO_PL = DS_CONTA,
#                                                 VALOR_PASSIVO_PL = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreReceitaLiq, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_RECEITALIQ = ESCALA_MOEDA,
#                                                 COD_DRE_RECEITALIQ = CD_CONTA,
#                                                 NOME_DRE_RECEITALIQ = DS_CONTA,
#                                                 VALOR_DRE_RECEITALIQ = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreCMV, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_CMV = ESCALA_MOEDA,
#                                                 COD_DRE_CMV = CD_CONTA,
#                                                 NOME_DRE_CMV = DS_CONTA,
#                                                 VALOR_DRE_CMV = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreLucroBruto, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_LUCROBRUTO = ESCALA_MOEDA,
#                                                 COD_DRE_LUCROBRUTO = CD_CONTA,
#                                                 NOME_DRE_LUCROBRUTO = DS_CONTA,
#                                                 VALOR_DRE_LUCROBRUTO = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreLucroOp, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_LUCROOP = ESCALA_MOEDA,
#                                                 COD_DRE_LUCROOP = CD_CONTA,
#                                                 NOME_DRE_LUCROOP = DS_CONTA,
#                                                 VALOR_DRE_LUCROOP = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreResultFin, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_RESULTFIN = ESCALA_MOEDA,
#                                                 COD_DRE_RESULTFIN = CD_CONTA,
#                                                 NOME_DRE_RESULTFIN = DS_CONTA,
#                                                 VALOR_DRE_RESULTFIN = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreDespFin, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_DESPFIN = ESCALA_MOEDA,
#                                                 COD_DRE_DESPFIN = CD_CONTA,
#                                                 NOME_DRE_DESPFIN = DS_CONTA,
#                                                 VALOR_DRE_DESPFIN = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreTrib, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_TRIB = ESCALA_MOEDA,
#                                                 COD_DRE_TRIB = CD_CONTA,
#                                                 NOME_DRE_TRIB = DS_CONTA,
#                                                 VALOR_DRE_TRIB = VL_CONTA)))
# teste.df <- distinct(full_join(teste.df, select(dreLucroLiq, CD_CVM, DT_REFER,
#                                                 ESCALA_MOEDA_DRE_LUCROLIQ = ESCALA_MOEDA,
#                                                 COD_DRE_LUCROLIQ = CD_CONTA,
#                                                 NOME_DRE_LUCROLIQ = DS_CONTA,
#                                                 VALOR_DRE_LUCROLIQ = VL_CONTA)))
# teste.df <- subset(teste.df, month(ymd(teste.df$DT_REFER)) == 12)
# teste.df <- distinct(left_join(teste.df, select(cia.info2, CD_CVM = id.company,
#                                                 tickers)))
#
#
#
# cvm <- unique(na.omit(select(teste.df, CD_CVM, DT_REFER, tickers)))
#
# colunas <- paste0("tick", 1:(max(str_count(cvm$tickers, ';')) + 1))
#
# cvm <- separate(cvm, tickers, into = colunas, sep = ';', remove = FALSE,
#          extra = "merge")
# cvm$tickers <- NULL
#
# write.csv2(teste.df, "Data/df1.csv", row.names = F)


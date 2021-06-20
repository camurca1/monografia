rm(list = ls())
options(scipen = 999)


library(GetDFPData2)
library(GetDFPData)
library(GetFREData)
library(tidyverse)
library(tidyr)
library(lubridate)

cia.info <- tibble(get_info_companies(tempdir()))
cia.info2 <- tibble(gdfpd.get.info.companies(type.data = 'companies',
                                             cache.folder = tempdir()))
saveRDS(cia.info, file = "Data/cia_info")
saveRDS(cia.info2, file = "Data/cia_info2")

cia.info$DT_REG <-dmy(cia.info$DT_REG)

cias.regulares <- cia.info %>%
  filter(is.na(DT_CANCEL)) %>%
  filter(SIT_REG=="ATIVO") %>%
  filter(SIT_EMISSOR!="EM LIQUIDAÇÃO JUDICIAL") %>%
  filter(SIT_EMISSOR!="LIQUIDAÇÃO EXTRAJUDICIAL") %>%
  filter(SIT_EMISSOR!="EM RECUPERAÇÃO EXTRAJUDICIAL") %>%
  filter(SIT_EMISSOR!="EM RECUPERAÇÃO JUDICIAL OU EQUIVALENTE") %>%
  filter(SIT_EMISSOR!="PARALISADA") %>%
  filter(SIT_EMISSOR!="FALIDA") %>%
  filter(DT_REG < "2015-01-01")
saveRDS(cias.regulares, file = "Data/cias_regulares")


codigos.cvm <- c(t(cias.regulares$CD_CVM))
codigos.cvm <- codigos.cvm[-match(c(13773,23175,21636), codigos.cvm)]

cia.info2_reduzido <- left_join(as.data.frame(codigos.cvm),
                                cia.info2,
                                by = c("codigos.cvm" = "id.company"))
cia.info2_reduzido <- cia.info2_reduzido %>%
  filter(!is.na(tickers)) %>%
  filter(codigos.cvm != 12653)
saveRDS(cia.info2_reduzido, file = "Data/cia_info_reduzido")


tickers <- as.data.frame(cia.info2_reduzido$codigos.cvm)
names(tickers)[1] <- "CD_CVM"
tickers$symbols <- cia.info2_reduzido$tickers
tickers <- tickers %>%
  separate(symbols,
           sep = ";",
           into = c("sym1", "sym2", "sym3"),
           convert = T) %>%
  pivot_longer(cols = sym1:sym3,
               names_to = "sym",
               values_to = "simbolo") %>%
  na.omit(simbolo)
tickers$sym <- NULL
saveRDS(tickers, "Data/tabelatickers")

codigos.cvm <- c(t(cia.info2_reduzido$codigos.cvm))

l.dfp <- get_dfp_data(companies_cvm_codes = codigos.cvm,
                      use_memoise = FALSE,
                      clean_data = TRUE,
                      type_docs = c('BPA', 'BPP', 'DRE', 'DFC_MI'),
                      type_format = 'con',
                      first_year = 2015,
                      last_year = 2020)
saveRDS(l.dfp, file = "Data/cia_dfp")


l.fre <- get_fre_data(companies_cvm_codes = codigos.cvm,
                      fre_to_read = 'last',
                      first_year = 2015,
                      last_year = 2021)
saveRDS(l.fre, file = "Data/cia_fre")


rm(list = ls())
gc()

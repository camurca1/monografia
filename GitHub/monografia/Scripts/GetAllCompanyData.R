rm(list = ls())
options(scipen = 999)

library(GetDFPData2)
library(GetDFPData)
library(GetFREData)
library(tidyverse)
library(quantmod)

cia.info <- tibble(get_info_companies(tempdir()))
cia.info2 <- tibble(gdfpd.get.info.companies(type.data = 'companies', cache.folder = tempdir()))

saveRDS(cia.info, file = "Data/cia_info")
saveRDS(cia.info2, file = "Data/cia_info2")

cvm.code <- c(t(cia.info$CD_CVM))

l.dfp <- get_dfp_data(companies_cvm_codes = cvm.code, use_memoise = FALSE, 
                      clean_data = TRUE, type_docs = c('BPA', 'BPP', 'DMPL', 'DRE'), 
                      type_format = 'con', first_year = 2010, last_year = 2020)
saveRDS(l.dfp, file = "Data/cia_dfp") 


l.fre <- get_fre_data(companies_cvm_codes = cvm.code[-c(426,683,801,834,836)],
                      fre_to_read = 'last',
                      first_year = 2010,
                      last_year = 2020)

saveRDS(l.fre, file = "Data/cia_fre")
l.fre <- readRDS("Data/cia_fre")

tickers <- c(t(na.omit(cia.info2$tickers)))
tickers <- unlist((strsplit(tickers, ";")))

saveRDS(tickers, file = "Data/cia_tickers")

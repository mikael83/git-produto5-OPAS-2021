#####################################################################
############ Script de mapas painéis 2021        ####################
########### V.1.0 - Desenvolvido por Mikael Lemos ####################
######################################################################

#### Carregando bibliotecas ####

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

#install.packages('Amelia')
library('Amelia')

# install.packages("tidyverse")
library(tidyverse)

# install.packages("lubridate")
library(lubridate)

# install.packages("ggplot2")
library(ggplot2)

#install.packages("read.dbc")

library(read.dbc)

#install.packages("forcats")

#library(forcats)

library("foreign")

#install.packages("foreign")

#install.packages("openxlsx")

library("openxlsx")

library(RColorBrewer)

library(Amelia)
library(rlang)

#install.packages("forcats")
library(forcats)

library(readr)

#install.packages("fs")

library(fs)

#install.packages("stringi")
library(stringi)

#install.packages("readxl")
library(readxl)

#install.packages("writexl")
library(writexl)


# carregar dados geolocalização IBGE

BR_localidaades <- read.dbf("C:/Users/lemos/Downloads/GEO_IBGE/BR_Localidades_2010_v1.dbf")

colnames(BR_localidaades)

BR_localidades_selec <- select(BR_localidaades, CD_GEOCODM, NM_MUNICIP, NM_UF, LONG, LAT, ALT)

BR_localidades_selec$CD_GEOCODM <- as.character(BR_localidades_selec$CD_GEOCODM)

BR_localidades_selec_un <- distinct(BR_localidades_selec, CD_GEOCODM , .keep_all = TRUE)


###########
### TAXAS - REG, UF, MUN
###########

#tx_hepc_sinan_2001_2019_reg <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/DEMANDAS/TX_INCIDÊNCIA_HEPC/mapa_tx_regioes_hepc_2000_2019_cenario2.xlsx")

tx_hepc_sinan_2001_2019_uf <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/DEMANDAS/TX_INCIDÊNCIA_HEPC/mapa_tx_uf_hepc_2000_2019_cenario2.xlsx")

tx_hepc_sinan_2001_2019_uf <- select(tx_hepc_sinan_2001_2019_uf, NM_UF = UF, ano, tx_tota = total)

tx_mapa_hepc__sinan_2001_2019_uf <- inner_join(tx_hepc_sinan_2001_2019_uf, BR_localidades_selec_un,by = "NM_UF" )

tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "RO"] <- "RONDÔNIA"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "AC"] <- "ACRE"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "AM"] <- "AMAZONAS"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "RR"] <- "RORAIMA"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "PA"] <- "PARÁ"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "AP"] <- "AMAPÁ"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "TO"] <- "TOCANTINS"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "MA"] <- "MARANHÃO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "PI"] <- "PIAUÍ"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "CE"] <- "CEARÁ"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "RN"] <- "RIO GRANDE DO NORTE"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "PB"] <- "PARAÍBA"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "PE"] <- "PERNAMBUCO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "AL"] <- "ALAGOAS"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "SE"] <- "SERGIPE"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "BA"] <- "BAHIA"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "MG"] <- "MINAS GERAIS"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "ES"] <- "ESPÍRITO SANTO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "RJ"] <- "RIO DE JANEIRO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "SP"] <- "SÃO PAULO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "PR"] <- "PARANÁ"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "SC"] <- "SANTA CATARINA"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "RS"] <- "RIO GRANDE DO SUL"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "MS"] <- "MATO GROSSO DO SUL"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "MT"] <- "MATO GROSSO"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "GO"] <- "GOIÁS"
tx_hepc_sinan_2001_2019_uf$NM_UF[tx_hepc_sinan_2001_2019_uf$NM_UF == "DF"] <- "DISTRITO FEDERAL"

tx_mapa_hepc__sinan_2001_2019_uf_un <- distinct(tx_mapa_hepc__sinan_2001_2019_uf, NM_UF, ano , .keep_all = TRUE)

write.xlsx(tx_mapa_hepc__sinan_2001_2019_uf_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/tx_mapa_hepc__sinan_2001_2019_uf_un.xlsx")

write.csv(tx_mapa_hepc__sinan_2001_2019_uf_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/tx_mapa_hepc__sinan_2001_2019_uf_un.csv")

tx_hepc_sinan_2001_2019_mun_100 <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/DEMANDAS/TX_INCIDÊNCIA_HEPC/tx_hepc_mun_2001_2019_100_cen2.xlsx")

tx_hepc_sinan_2001_2019_mun_100 <- select(tx_hepc_sinan_2001_2019_mun_100, CD_GEOCODM = cod, NM_MUNICIP = municipio.x, ano = ano.x, tx_total_ano = taxa.de.detecção  )

BR_localidades_selec_un$CD_GEOCODM <- as.numeric(BR_localidades_selec_un$CD_GEOCODM)

tx_hepc_sinan_2001_2019_mun_100 <- inner_join(tx_hepc_sinan_2001_2019_mun_100, BR_localidades_selec_un,by = "CD_GEOCODM" )

write.xlsx(tx_hepc_sinan_2001_2019_mun_100, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/tx_hepc_sinan_2001_2019_mun_100.xlsx")

write.csv(tx_hepc_sinan_2001_2019_mun_100, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/tx_hepc_sinan_2001_2019_mun_100.csv")

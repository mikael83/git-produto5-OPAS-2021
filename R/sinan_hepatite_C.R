######################################################################
############ Script de atualização do painel 2020 ####################
###########         Painel de hapatites C         ####################
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

##################
##################
### HEPATITE C ###
##### 2020 #######
##################

##############################
#### Carregando banco SINAN - hepatite C

sinan_hepatite_C <- read.dbf("C:/Users/lemos/Downloads/HEPATITES/BANCOS/SINAN_HEPATITES_HIV_2020_PRELIMINAR/HEPAN20CI.DBF")

## Checando preenchimento 
sinan_hep_C_datas <- select(sinan_hepatite_C, DT_NOTIFIC, DT_SIN_PRI,DT_DIGITA, DT_TRANSUS, DT_TRANSDM, DT_TRANSSM, DT_TRANSRM, DT_TRANSRS, DT_TRASSE, DT_INVEST, DT_ENCERRA, NU_ANO )
sinan_hep_C_fonte <- select(sinan_hepatite_C, FONTE )
sinan_hep_C_hemodialise <- select(sinan_hepatite_C, HEMODIALIS )
sinan_hep_C_cirurgico <- select(sinan_hepatite_C, CIRURGICO)
sinan_hep_C_injetaveis <- select(sinan_hepatite_C, INJETAVEIS)
sinan_hep_C_transfusao <- select(sinan_hepatite_C, TRANSFUSAO)
sinan_hep_C_acunputura <- select(sinan_hepatite_C, ACUPUNTURA)
sinan_hep_C_inal_crack <- select(sinan_hepatite_C, INAL_CRACK)
sinan_hep_C_tatu_pierc <- select(sinan_hepatite_C, TATU_PIER)
sinan_hep_C_outras_DST <- select(sinan_hepatite_C, OUTRA_DST)
sinan_hep_C_idade <- select(sinan_hepatite_C, NU_IDADE_N)
sinan_hep_C_id_localiz <- select(sinan_hepatite_C, ID_MUNICIP,ID_MN_RESI, ID_RG_RESI ,ID_PAIS, ID_GEO1, ID_GEO2)
sinan_hep_C_uf <- select(sinan_hepatite_C, CO_UF_EXP, CO_UF_EX2, CO_UF_EX3)
sinan_hep_C_mun <- select(sinan_hepatite_C, CO_MUN_EXP, CO_MUN_EX2, CO_MUN_EX3)
sinan_hep_C_unidade <- select(sinan_hepatite_C, ID_UNIDADE)
sinan_hep_C_raca <- select(sinan_hepatite_C, CS_RACA)
sinan_hep_C_gestantes <- select(sinan_hepatite_C, CS_GESTANT)
sinan_hep_C_escolaridade <- select(sinan_hepatite_C, CS_ESCOL_N)
sinan_hep_C_ocupacao <- select(sinan_hepatite_C,OCUPACIO)
sinan_hep_C_vacina_A <- select(sinan_hepatite_C,HEPATITA)
sinan_hep_C_vacina_B <- select(sinan_hepatite_C,HEPATITB)
sinan_hep_C_forma <- select(sinan_hepatite_C,FORMA)
sinan_hep_C_HIV <- select(sinan_hepatite_C,HIV)
sinan_hep_C_tres_mais <- select(sinan_hepatite_C, TRESMAIS)

missmap(sinan_hep_C_tres_mais)

##### Painel - Informações do SINAN - Causa da infecção, Sexo, Faixa etária

#sinan_hepatite_C_sel1 <- select(sinan_hepatite_C, NOME, NOMEMAE, DTNASC, MUNRES, SEXO, ID_MUNICIP, ID_UNIDADE, NM_PACIENT, NU_IDADE_N, CS_SEXO_NE, CS_RACA_NE, CS_CNS_SUS, HIV_NET, OUTRA_DST, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N, TRESMAIS_N, HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN,FORMA_NET,CLAS_ETIOL, DSFONTE_NE, SEXO_WIN, IDADE_WIN, RACA_WIN,FORMA_WIN, FONTE_WIN, ANTIHAVI_A, ALCOOLISMO, TRANSFUS_A, ACUPUNTU_A, HEMODIAL_A, EXPOSICAO, TATUAGEM_W, INJETAVE_A, INJETAVE_A, CIRURGIC_A, PIERCING_W, INALAVEIS, DENTARIO_W, TRANSPLA_W, ACIDPERC_W, TRESMAIS_W, PARTO_WIN, PARCEIROS, DST_WIN, SEXUAL_WIN, ANTIHAVIGG, ano_notificacao  )

#missmap(sinan_hepatite_C_sel1)

sinan_hepatite_C_sel2 <- select(sinan_hepatite_C, NM_PACIENT, NU_NOTIFIC, NM_MAE_PAC, DT_NASC, ID_MUNICIP, CS_SEXO,ID_PAIS, ID_UNIDADE, NU_IDADE_N, CS_GESTANT, CS_RACA, HIV, OUTRA_DST, SEXUAL, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO, TRESMAIS, HEMODIALIS, TRANSPLA, OUTRAS, CLASSI_FIN,FORMA,CLAS_ETIOL, NU_ANO, DT_NOTIFIC, FONTE, CS_ESCOL_N, OCUPACIO, HEPATITA, HEPATITB,HEPATITE_N )

missmap(sinan_hepatite_C_sel2)

colnames(sinan_hepatite_C_sel2)

######
## Informações de mês
######

sinan_hepatite_C_sel2$mes <- as.numeric(format(sinan_hepatite_C_sel2$DT_NOTIFIC,'%m'))

##########################################

###############
#### contagem e porcentagem
##############

sinan_hepatite_C_sel2 <- sinan_hepatite_C_sel2 %>% group_by( NM_PACIENT, NU_NOTIFIC, NM_MAE_PAC, DT_NASC, ID_MUNICIP, CS_SEXO,ID_PAIS, ID_UNIDADE, NU_IDADE_N, CS_GESTANT, CS_RACA, HIV, OUTRA_DST, SEXUAL, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO, TRESMAIS, HEMODIALIS, TRANSPLA, OUTRAS, CLASSI_FIN,FORMA,CLAS_ETIOL, NU_ANO, DT_NOTIFIC, FONTE, CS_ESCOL_N, OCUPACIO, HEPATITA, HEPATITB,HEPATITE_N, mes   )
sinan_hepatite_C_sel2_n <- sinan_hepatite_C_sel2 %>% summarise(n = n())

### removendo duplicado

sinan_hepatite_C_sel2_un_n = sinan_hepatite_C_sel2_n

#sinan_hepatite_C_sel2_n_duplicado <- filter(sinan_hepatite_C_sel2_n, n==2)

#sinan_hepatite_C_sel2_un_n <- filter(sinan_hepatite_C_sel2_n, n!=2)

sinan_hepatite_C_sel2_un_n$n_pct = sinan_hepatite_C_sel2_un_n$n / sum(sinan_hepatite_C_sel2_un_n$n) 

##### Substituindo códigos por informações

sinan_hepatite_C_sel2_un_n$CS_RACA  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CS_RACA )
sinan_hepatite_C_sel2_un_n$CS_RACA <- as.character(sinan_hepatite_C_sel2_un_n$CS_RACA)

sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "1"] <- "Branca"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "2"] <- "Preta"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "3"] <- "Amarela"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "4"] <- "Parda"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "5"] <- "Indígena"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CS_RACA[sinan_hepatite_C_sel2_un_n$CS_RACA == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$FONTE  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$FONTE )
sinan_hepatite_C_sel2_un_n$FONTE <- as.character(sinan_hepatite_C_sel2_un_n$FONTE)

sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "01"] <- "Sexual"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "02"] <- "Transfusional"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "03"] <- "Uso de Drogas"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "04"] <- "Vertical"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "05"] <- "Acidente de Trabalho"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "06"] <- "Hemodiálise"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "07"] <- "Domiciliar"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "08"] <- "Tratamento cirúrgico"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "09"] <- "Tratamento dentário"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "10"] <- "Pessoa/pessoa"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "11"] <- "Alimento/água"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "12"] <- "Outros"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "99"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$FONTE[sinan_hepatite_C_sel2_un_n$FONTE == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$ID_PAIS  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$ID_PAIS )
sinan_hepatite_C_sel2_un_n$ID_PAIS <- as.character(sinan_hepatite_C_sel2_un_n$ID_PAIS)

sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "001"] <- "Brasil"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "1"] <- "Brasil"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "016"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "022"] <- "Bolívia"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "033"] <- "Aruba"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "105"] <- "Holanda"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "107"] <- "Ilhas Baleares"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "114"] <- "Iugoslávia"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "126"] <- "Suécia"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "137"] <- "Montenegro"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "138"] <- "Azerbaijão"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "151"] <- "Kalmir"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "156"] <- "Lituânia"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "184"] <- "Ilhas Comores"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "199"] <- "Ilhas Canárias"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "21"] <- "Argentina"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "247"] <- "Catar"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "253"] <- "Hong Kong"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "261"] <- "Camboja"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "31"] <- "Bélgica"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "32"] <- "Reino Unido"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "46"] <- "Belize"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "40"] <- "Comunidade das Bahamas"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "68"] <- "Martinica"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "7"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "74"] <- "Porto Rico"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "800"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$ID_PAIS[sinan_hepatite_C_sel2_un_n$ID_PAIS == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$TATU_PIER  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$TATU_PIER )
sinan_hepatite_C_sel2_un_n$TATU_PIER <- as.character(sinan_hepatite_C_sel2_un_n$TATU_PIER)

sinan_hepatite_C_sel2_un_n$TATU_PIER[sinan_hepatite_C_sel2_un_n$TATU_PIER == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$TATU_PIER[sinan_hepatite_C_sel2_un_n$TATU_PIER == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$TATU_PIER[sinan_hepatite_C_sel2_un_n$TATU_PIER == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$TATU_PIER[sinan_hepatite_C_sel2_un_n$TATU_PIER == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$TATU_PIER[sinan_hepatite_C_sel2_un_n$TATU_PIER == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$INAL_CRACK  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$INAL_CRACK )
sinan_hepatite_C_sel2_un_n$INAL_CRACK <- as.character(sinan_hepatite_C_sel2_un_n$INAL_CRACK)

sinan_hepatite_C_sel2_un_n$INAL_CRACK[sinan_hepatite_C_sel2_un_n$INAL_CRACK == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$INAL_CRACK[sinan_hepatite_C_sel2_un_n$INAL_CRACK == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$INAL_CRACK[sinan_hepatite_C_sel2_un_n$INAL_CRACK == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$INAL_CRACK[sinan_hepatite_C_sel2_un_n$INAL_CRACK == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$INAL_CRACK[sinan_hepatite_C_sel2_un_n$INAL_CRACK == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$ACUPUNTURA  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$ACUPUNTURA )
sinan_hepatite_C_sel2_un_n$ACUPUNTURA <- as.character(sinan_hepatite_C_sel2_un_n$ACUPUNTURA)

sinan_hepatite_C_sel2_un_n$ACUPUNTURA[sinan_hepatite_C_sel2_un_n$ACUPUNTURA == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$ACUPUNTURA[sinan_hepatite_C_sel2_un_n$ACUPUNTURA == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$ACUPUNTURA[sinan_hepatite_C_sel2_un_n$ACUPUNTURA == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$ACUPUNTURA[sinan_hepatite_C_sel2_un_n$ACUPUNTURA == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$ACUPUNTURA[sinan_hepatite_C_sel2_un_n$ACUPUNTURA == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$TRANSFUSAO  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$TRANSFUSAO )
sinan_hepatite_C_sel2_un_n$TRANSFUSAO <- as.character(sinan_hepatite_C_sel2_un_n$TRANSFUSAO)

sinan_hepatite_C_sel2_un_n$TRANSFUSAO[sinan_hepatite_C_sel2_un_n$TRANSFUSAO == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$TRANSFUSAO[sinan_hepatite_C_sel2_un_n$TRANSFUSAO == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$TRANSFUSAO[sinan_hepatite_C_sel2_un_n$TRANSFUSAO == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$TRANSFUSAO[sinan_hepatite_C_sel2_un_n$TRANSFUSAO == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$TRANSFUSAO[sinan_hepatite_C_sel2_un_n$TRANSFUSAO == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$INJETAVEIS  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$INJETAVEIS )
sinan_hepatite_C_sel2_un_n$INJETAVEIS <- as.character(sinan_hepatite_C_sel2_un_n$INJETAVEIS)

sinan_hepatite_C_sel2_un_n$INJETAVEIS[sinan_hepatite_C_sel2_un_n$INJETAVEIS == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$INJETAVEIS[sinan_hepatite_C_sel2_un_n$INJETAVEIS == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$INJETAVEIS[sinan_hepatite_C_sel2_un_n$INJETAVEIS == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$INJETAVEIS[sinan_hepatite_C_sel2_un_n$INJETAVEIS == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$INJETAVEIS[sinan_hepatite_C_sel2_un_n$INJETAVEIS == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CIRURGICO  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CIRURGICO )
sinan_hepatite_C_sel2_un_n$CIRURGICO <- as.character(sinan_hepatite_C_sel2_un_n$CIRURGICO)

sinan_hepatite_C_sel2_un_n$CIRURGICO[sinan_hepatite_C_sel2_un_n$CIRURGICO == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$CIRURGICO[sinan_hepatite_C_sel2_un_n$CIRURGICO == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$CIRURGICO[sinan_hepatite_C_sel2_un_n$CIRURGICO == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$CIRURGICO[sinan_hepatite_C_sel2_un_n$CIRURGICO == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CIRURGICO[sinan_hepatite_C_sel2_un_n$CIRURGICO == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$DENTARIO  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$DENTARIO )
sinan_hepatite_C_sel2_un_n$DENTARIO <- as.character(sinan_hepatite_C_sel2_un_n$DENTARIO)

sinan_hepatite_C_sel2_un_n$DENTARIO[sinan_hepatite_C_sel2_un_n$DENTARIO == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$DENTARIO[sinan_hepatite_C_sel2_un_n$DENTARIO == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$DENTARIO[sinan_hepatite_C_sel2_un_n$DENTARIO == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$DENTARIO[sinan_hepatite_C_sel2_un_n$DENTARIO == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$DENTARIO[sinan_hepatite_C_sel2_un_n$DENTARIO == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$TRESMAIS  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$TRESMAIS )
sinan_hepatite_C_sel2_un_n$TRESMAIS <- as.character(sinan_hepatite_C_sel2_un_n$TRESMAIS)

sinan_hepatite_C_sel2_un_n$TRESMAIS[sinan_hepatite_C_sel2_un_n$TRESMAIS == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$TRESMAIS[sinan_hepatite_C_sel2_un_n$TRESMAIS == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$TRESMAIS[sinan_hepatite_C_sel2_un_n$TRESMAIS == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$TRESMAIS[sinan_hepatite_C_sel2_un_n$TRESMAIS == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$TRESMAIS[sinan_hepatite_C_sel2_un_n$TRESMAIS == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$HEMODIALIS  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HEMODIALIS )
sinan_hepatite_C_sel2_un_n$HEMODIALIS <- as.character(sinan_hepatite_C_sel2_un_n$HEMODIALIS)

sinan_hepatite_C_sel2_un_n$HEMODIALIS[sinan_hepatite_C_sel2_un_n$HEMODIALIS == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$HEMODIALIS[sinan_hepatite_C_sel2_un_n$HEMODIALIS == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$HEMODIALIS[sinan_hepatite_C_sel2_un_n$HEMODIALIS == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$HEMODIALIS[sinan_hepatite_C_sel2_un_n$HEMODIALIS == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HEMODIALIS[sinan_hepatite_C_sel2_un_n$HEMODIALIS == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$TRANSPLA  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$TRANSPLA )
sinan_hepatite_C_sel2_un_n$TRANSPLA <- as.character(sinan_hepatite_C_sel2_un_n$TRANSPLA)

sinan_hepatite_C_sel2_un_n$TRANSPLA[sinan_hepatite_C_sel2_un_n$TRANSPLA == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$TRANSPLA[sinan_hepatite_C_sel2_un_n$TRANSPLA == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$TRANSPLA[sinan_hepatite_C_sel2_un_n$TRANSPLA == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$TRANSPLA[sinan_hepatite_C_sel2_un_n$TRANSPLA == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$TRANSPLA[sinan_hepatite_C_sel2_un_n$TRANSPLA == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$HEPATITA  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HEPATITA )
sinan_hepatite_C_sel2_un_n$HEPATITA <- as.character(sinan_hepatite_C_sel2_un_n$HEPATITA)

sinan_hepatite_C_sel2_un_n$HEPATITA[sinan_hepatite_C_sel2_un_n$HEPATITA == "1"] <- "Completa"
sinan_hepatite_C_sel2_un_n$HEPATITA[sinan_hepatite_C_sel2_un_n$HEPATITA == "2"] <- "Incompleta"
sinan_hepatite_C_sel2_un_n$HEPATITA[sinan_hepatite_C_sel2_un_n$HEPATITA == "3"] <- "Não vacinado"
sinan_hepatite_C_sel2_un_n$HEPATITA[sinan_hepatite_C_sel2_un_n$HEPATITA == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HEPATITA[sinan_hepatite_C_sel2_un_n$HEPATITA == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$HEPATITB  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HEPATITB )
sinan_hepatite_C_sel2_un_n$HEPATITB <- as.character(sinan_hepatite_C_sel2_un_n$HEPATITB)

sinan_hepatite_C_sel2_un_n$HEPATITB[sinan_hepatite_C_sel2_un_n$HEPATITB == "1"] <- "Completa"
sinan_hepatite_C_sel2_un_n$HEPATITB[sinan_hepatite_C_sel2_un_n$HEPATITB == "2"] <- "Incompleta"
sinan_hepatite_C_sel2_un_n$HEPATITB[sinan_hepatite_C_sel2_un_n$HEPATITB == "3"] <- "Não vacinado"
sinan_hepatite_C_sel2_un_n$HEPATITB[sinan_hepatite_C_sel2_un_n$HEPATITB == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HEPATITB[sinan_hepatite_C_sel2_un_n$HEPATITB == "(Missing)" ] <- "Ignorado"

##### Substituição código IBGE município por nome do Município

mun_Brasil <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto4/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx")

mun_Brasil <- select(mun_Brasil, ID_MUNICIP = Código.Município.Completo, Nome_Município)

mun_Brasil$ID_MUNICIP = substr(mun_Brasil$ID_MUNICIP,1,nchar(mun_Brasil$ID_MUNICIP)-1)

sinan_hepatite_C_sel2_un_n$ID_MUNICIP <- as.character(sinan_hepatite_C_sel2_un_n$ID_MUNICIP)
mun_Brasil$ID_MUNICIP <- as.character(mun_Brasil$ID_MUNICIP)

sinan_hepatite_C_sel2_un_n <- inner_join(sinan_hepatite_C_sel2_un_n, mun_Brasil, by = "ID_MUNICIP" )

### UF a parir do município de residência

sinan_hepatite_C_sel2_un_n$UF <- substr(sinan_hepatite_C_sel2_un_n$ID_MUNICIP, 0, 2)

sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "11"] <- "Rondônia"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "12"] <- "Acre"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "13"] <- "Amazonas"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "14"] <- "Roraima"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "15"] <- "Pará"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "16"] <- "Amapá"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "17"] <- "Tocantins"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "21"] <- "Maranhão"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "22"] <- "Piauí"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "23"] <- "Ceará"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "24"] <- "Rio Grande do Norte"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "25"] <- "Paraíba"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "26"] <- "Pernambuco"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "27"] <- "Alagoas"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "28"] <- "Sergipe"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "29"] <- "Bahia"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "31"] <- "Minas Gerais"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "32"] <- "Espírito Santo"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "33"] <- "Rio de Janeiro"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "35"] <- "São Paulo"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "41"] <- "Paraná"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "42"] <- "Santa Catarina"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "43"] <- "Rio Grande do Sul"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "50"] <- "Mato Grosso do Sul"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "51"] <- "Mato Grosso"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "52"] <- "Goiás"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "53"] <- "Distrito Federal"

sinan_hepatite_C_sel2_un_n$CS_GESTANT  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CS_GESTANT )
sinan_hepatite_C_sel2_un_n$CS_GESTANT <- as.character(sinan_hepatite_C_sel2_un_n$CS_GESTANT)

sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "1"] <- "1ºTrimestre"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "2"] <- "2ºTrimestre"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "3"] <- "3ºTrimestre"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "4"] <- "Idade gestacional Ignorada"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "5"] <- "Não"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "6"] <- "Não se aplica"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CS_GESTANT[sinan_hepatite_C_sel2_un_n$CS_GESTANT == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$SEXUAL  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$SEXUAL )
sinan_hepatite_C_sel2_un_n$SEXUAL <- as.character(sinan_hepatite_C_sel2_un_n$SEXUAL)

sinan_hepatite_C_sel2_un_n$SEXUAL[sinan_hepatite_C_sel2_un_n$SEXUAL == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$SEXUAL[sinan_hepatite_C_sel2_un_n$SEXUAL == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$SEXUAL[sinan_hepatite_C_sel2_un_n$SEXUAL == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$SEXUAL[sinan_hepatite_C_sel2_un_n$SEXUAL == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$SEXUAL[sinan_hepatite_C_sel2_un_n$SEXUAL == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$OCUPACIO  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$OCUPACIO )
sinan_hepatite_C_sel2_un_n$OCUPACIO <- as.character(sinan_hepatite_C_sel2_un_n$OCUPACIO)

sinan_hepatite_C_sel2_un_n$OCUPACIO[sinan_hepatite_C_sel2_un_n$OCUPACIO == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$OCUPACIO[sinan_hepatite_C_sel2_un_n$OCUPACIO == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$OCUPACIO[sinan_hepatite_C_sel2_un_n$OCUPACIO == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$OCUPACIO[sinan_hepatite_C_sel2_un_n$OCUPACIO == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$OCUPACIO[sinan_hepatite_C_sel2_un_n$OCUPACIO == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$FORMA  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$FORMA )
sinan_hepatite_C_sel2_un_n$FORMA <- as.character(sinan_hepatite_C_sel2_un_n$FORMA)

sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "1"] <- "Hepatite Aguda"
sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "2"] <- "Hepatite Crônica/Portador Assintomático"
sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "3"] <- "Hepatite Fulminante"
sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "4"] <- "Inconclusivo"
sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$FORMA[sinan_hepatite_C_sel2_un_n$FORMA == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CS_ESCOL_N  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CS_ESCOL_N )
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N <- as.character(sinan_hepatite_C_sel2_un_n$CS_ESCOL_N)

sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "0"] <- "Analfabeto"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "00"] <- "Analfabeto"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "1"] <- "1ª a 4ª série incompleta do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "01"] <- "1ª a 4ª série incompleta do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "2"] <- "4ª série completa do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "02"] <- "4ª série completa do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "3"] <- "5ª à 8ª série incompleta do EF (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "03"] <- "5ª à 8ª série incompleta do EF (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "4"] <- "Ensino fundamental completo (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "04"] <- "Ensino fundamental completo (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "5"] <- "5-Ensino médio incompleto (antigo colegial ou 2º grau )"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "05"] <- "5-Ensino médio incompleto (antigo colegial ou 2º grau )"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "6"] <- "Ensino médio completo (antigo colegial ou 2º grau ) "
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "06"] <- "Ensino médio completo (antigo colegial ou 2º grau ) "
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "7"] <- "Educação superior incompleta"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "07"] <- "Educação superior incompleta"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "8"] <- "Educação superior completa"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "08"] <- "Educação superior completa"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "09"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "10"] <- " Não se aplica"
sinan_hepatite_C_sel2_un_n$CS_ESCOL_N[sinan_hepatite_C_sel2_un_n$CS_ESCOL_N == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CLAS_ETIOL  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CLAS_ETIOL )
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL <- as.character(sinan_hepatite_C_sel2_un_n$CLAS_ETIOL)

sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "01"] <- "Vírus A"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "02"] <- "Vírus B"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "03"] <- "Vírus C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "04"] <- "Vírus B e D"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "05"] <- "Vírus E"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "06"] <- "Vírus B e C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "07"] <- "Vírus A e B"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "08"] <- "Vírus A e C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "09"] <- "Outras hepatites virais"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "99"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CLASSI_FIN  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CLASSI_FIN )
sinan_hepatite_C_sel2_un_n$CLASSI_FIN <- as.character(sinan_hepatite_C_sel2_un_n$CLASSI_FIN)

sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "1"] <- "Confirmação laboratorial"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "2"] <- "Confirmação clínico-epidemiológica"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "3"] <- "Descartado"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "4"] <- "Cicatriz sorológica"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "8"] <- "Inconclusivo"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$HIV  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HIV )
sinan_hepatite_C_sel2_un_n$HIV <- as.character(sinan_hepatite_C_sel2_un_n$HIV)

sinan_hepatite_C_sel2_un_n$HIV[sinan_hepatite_C_sel2_un_n$HIV == "1"] <- "Sim"
sinan_hepatite_C_sel2_un_n$HIV[sinan_hepatite_C_sel2_un_n$HIV == "2"] <- "Não"
sinan_hepatite_C_sel2_un_n$HIV[sinan_hepatite_C_sel2_un_n$HIV == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HIV[sinan_hepatite_C_sel2_un_n$HIV == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$OUTRA_DST  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$OUTRA_DST )
sinan_hepatite_C_sel2_un_n$OUTRA_DST <- as.character(sinan_hepatite_C_sel2_un_n$OUTRA_DST)

sinan_hepatite_C_sel2_un_n$OUTRA_DST[sinan_hepatite_C_sel2_un_n$OUTRA_DST == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$OUTRA_DST[sinan_hepatite_C_sel2_un_n$OUTRA_DST == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$OUTRA_DST[sinan_hepatite_C_sel2_un_n$OUTRA_DST == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$OUTRA_DST[sinan_hepatite_C_sel2_un_n$OUTRA_DST == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$OUTRA_DST[sinan_hepatite_C_sel2_un_n$OUTRA_DST == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$MATBIOLOGI  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$MATBIOLOGI )
sinan_hepatite_C_sel2_un_n$MATBIOLOGI <- as.character(sinan_hepatite_C_sel2_un_n$MATBIOLOGI)

sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$AGUA_ALIME  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$AGUA_ALIME )
sinan_hepatite_C_sel2_un_n$AGUA_ALIME <- as.character(sinan_hepatite_C_sel2_un_n$AGUA_ALIME)

sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$OUTRAS  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$OUTRAS )
sinan_hepatite_C_sel2_un_n$OUTRAS <- as.character(sinan_hepatite_C_sel2_un_n$OUTRAS)

sinan_hepatite_C_sel2_un_n$OUTRAS[sinan_hepatite_C_sel2_un_n$OUTRAS == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$OUTRAS[sinan_hepatite_C_sel2_un_n$OUTRAS == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$OUTRAS[sinan_hepatite_C_sel2_un_n$OUTRAS == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$OUTRAS[sinan_hepatite_C_sel2_un_n$OUTRAS == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$OUTRAS[sinan_hepatite_C_sel2_un_n$OUTRAS == "(Missing)" ] <- "Ignorado"

## SUSPEITA

sinan_hepatite_C_sel2_un_n$HEPATITE_N  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HEPATITE_N )
sinan_hepatite_C_sel2_un_n$HEPATITE_N <- as.character(sinan_hepatite_C_sel2_un_n$HEPATITE_N)

sinan_hepatite_C_sel2_un_n$HEPATITE_N[sinan_hepatite_C_sel2_un_n$HEPATITE_N == "1"] <- "Hepatite A"
sinan_hepatite_C_sel2_un_n$HEPATITE_N[sinan_hepatite_C_sel2_un_n$HEPATITE_N == "2"] <- "Hepatite B/C"
sinan_hepatite_C_sel2_un_n$HEPATITE_N[sinan_hepatite_C_sel2_un_n$HEPATITE_N == "3"] <- "Não especificada"
sinan_hepatite_C_sel2_un_n$HEPATITE_N[sinan_hepatite_C_sel2_un_n$HEPATITE_N == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HEPATITE_N[sinan_hepatite_C_sel2_un_n$HEPATITE_N == "(Missing)" ] <- "Ignorado"


##### Cálculo de faixa etária por idade
sinan_hepatite_C_sel2_un_n$NU_IDADE_N <- as.character(sinan_hepatite_C_sel2_un_n$NU_IDADE_N)

sinan_hepatite_C_sel2_un_n$NU_IDADE_N = substr(sinan_hepatite_C_sel2_un_n$NU_IDADE_N,3,nchar(sinan_hepatite_C_sel2_un_n$NU_IDADE_N)-0)

sinan_hepatite_C_sel2_un_n$NU_IDADE_N <- as.numeric(sinan_hepatite_C_sel2_un_n$NU_IDADE_N)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

sinan_hepatite_C_sel2_un_n$IDADE_faixa <- cut(sinan_hepatite_C_sel2_un_n$NU_IDADE_N, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

# remover coluna NOME e NOMEMAE
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]

sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]

#sinan_hepatite_C_sel2_un_n <- select(sinan_hepatite_C_sel2_un_n, -NM_PACIENT)

write.csv(sinan_hepatite_C_sel2_un_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_2020.csv")

sinan_hepatite_C_sel2_un_n$n_pct <- round(sinan_hepatite_C_sel2_un_n$n_pct * 100, digits = 4)

write.xlsx(sinan_hepatite_C_sel2_un_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_2020.csv")

######################################
######################################
#######  SINAN - 1999-2019  ##########
#########   HEPATITE C  ##############
######################################
######################################

##############################
#### Carregando banco SINAN - hepatite C

sinan_hepatite_C <- read.dbf("C:/Users/lemos/Downloads/HEPATITES/BANCOS/SINAN_2020_A_B_C/Banco_hepatite_C_2020.dbf")

## Checando preenchimento 
sinan_hep_C_datas <- select(sinan_hepatite_C, DT_NOTIFIC, DT_SIN_PRI,DT_DIGITA, DT_TRANSUS, DT_TRANSDM, DT_TRANSSM, DT_TRANSRM, DT_TRANSRS, DT_TRASSE, DT_INVEST, DT_ENCERRA, DTNOT_WIN, DT_ENCER_A, NU_ANO_WIN, NU_ANO_NET )
sinan_hep_C_fonte <- select(sinan_hepatite_C, FONTE_NET, FONTE_WIN )
sinan_hep_C_hemodialise <- select(sinan_hepatite_C, HEMODIALIS, HEMODIAL_A )
sinan_hep_C_cirurgico <- select(sinan_hepatite_C, CIRURGICO, CIRURGIC_A )
sinan_hep_C_injetaveis <- select(sinan_hepatite_C, INJETAVEIS, INJETAVE_A )
sinan_hep_C_transfusao <- select(sinan_hepatite_C, TRANSFUSAO, TRANSFUS_A )
sinan_hep_C_acunputura <- select(sinan_hepatite_C, ACUPUNTURA, ACUPUNTU_A )
sinan_hep_C_inal_crack <- select(sinan_hepatite_C, INAL_CRACK, INALAVEIS )
sinan_hep_C_inal_crack <- select(sinan_hepatite_C, INAL_CRACK, INALAVEIS )
sinan_hep_C_tatu_pierc <- select(sinan_hepatite_C, TATU_PIER, TATUAGEM_W)
sinan_hep_C_outras_DST <- select(sinan_hepatite_C, OUTRA_DST, OUTRAS_NET)
sinan_hep_C_idade <- select(sinan_hepatite_C, NU_IDADE_N, IDADE_WIN)
sinan_hep_C_id_localiz <- select(sinan_hepatite_C, ID_MUNICIP, ID_MUNIC_A,ID_MN_RESI, ID_RG_RESI, ID_PAIS_NE ,ID_PAIS_WI, ID_GEO1_NE, ID_GEO2_NE)
sinan_hep_C_uf <- select(sinan_hepatite_C, UF_WIN)
sinan_hep_C_unidade <- select(sinan_hepatite_C, ID_UNIDADE, ID_UNIDA_A)
sinan_hep_C_raca <- select(sinan_hepatite_C, RACA_WIN, CS_RACA_NE)
sinan_hep_C_gestantes <- select(sinan_hepatite_C, GESTANTE_W, CS_GESTANT)
sinan_hep_C_escolaridade <- select(sinan_hepatite_C, CS_ESCOL_N, ESCOLARIDA)
sinan_hep_C_parto <- select(sinan_hepatite_C,PARTO_WIN)
sinan_hep_C_alcolismo <- select(sinan_hepatite_C,ALCOOLISMO)
sinan_hep_C_ocupacao <- select(sinan_hepatite_C,OCUPACIO_N,OCUPACIO_W, OCUPACAO_W)
sinan_hep_C_vacina_A <- select(sinan_hepatite_C,HEPATITA_N, HEPATITA_W)
sinan_hep_C_vacina_B <- select(sinan_hepatite_C,HEPATITB_N, HEPATITB_W)

missmap(sinan_hep_C_vacina_B)

## Unindo colunas complementares - data notificação
sinan_hepatite_C$NU_ANO_NET <- as.character(sinan_hepatite_C$NU_ANO_NET)
sinan_hepatite_C$NU_ANO_WIN <- as.character(sinan_hepatite_C$NU_ANO_WIN)

sinan_hepatite_C$FONTE_NET <- as.character(sinan_hepatite_C$FONTE_NET)
sinan_hepatite_C$FONTE_WIN <- as.character(sinan_hepatite_C$FONTE_WIN)

sinan_hepatite_C$FORMA_NET <- as.character(sinan_hepatite_C$FORMA_NET)
sinan_hepatite_C$FORMA_WIN <- as.character(sinan_hepatite_C$FORMA_WIN)

sinan_hepatite_C$TRANSPLA_N <- as.character(sinan_hepatite_C$TRANSPLA_N)
sinan_hepatite_C$TRANSPLA_W <- as.character(sinan_hepatite_C$TRANSPLA_W)

sinan_hepatite_C$HEMODIALIS <- as.character(sinan_hepatite_C$HEMODIALIS)
sinan_hepatite_C$HEMODIAL_A  <- as.character(sinan_hepatite_C$HEMODIAL_A )

sinan_hepatite_C$TRESMAIS_N <- as.character(sinan_hepatite_C$TRESMAIS_N)
sinan_hepatite_C$TRESMAIS_W  <- as.character(sinan_hepatite_C$TRESMAIS_W )

sinan_hepatite_C$DENTARIO_N <- as.character(sinan_hepatite_C$DENTARIO_N)
sinan_hepatite_C$DENTARIO_W  <- as.character(sinan_hepatite_C$DENTARIO_W )

sinan_hepatite_C$CIRURGICO <- as.character(sinan_hepatite_C$CIRURGICO)
sinan_hepatite_C$CIRURGIC_A <- as.character(sinan_hepatite_C$CIRURGIC_A)

sinan_hepatite_C$INJETAVEIS <- as.character(sinan_hepatite_C$INJETAVEIS)
sinan_hepatite_C$INJETAVE_A <- as.character(sinan_hepatite_C$INJETAVE_A)

sinan_hepatite_C$TRANSFUSAO <- as.character(sinan_hepatite_C$TRANSFUSAO)
sinan_hepatite_C$TRANSFUS_A  <- as.character(sinan_hepatite_C$TRANSFUS_A)

sinan_hepatite_C$INAL_CRACK <- as.character(sinan_hepatite_C$INAL_CRACK)
sinan_hepatite_C$INALAVEIS  <- as.character(sinan_hepatite_C$INALAVEIS)

sinan_hepatite_C$NU_IDADE_N <- as.character(sinan_hepatite_C$NU_IDADE_N)
sinan_hepatite_C$IDADE_WIN  <- as.character(sinan_hepatite_C$IDADE_WIN)

sinan_hepatite_C$ID_MUNIC_A <- as.character(sinan_hepatite_C$ID_MUNIC_A)
sinan_hepatite_C$ID_MUNICIP  <- as.character(sinan_hepatite_C$ID_MUNICIP)

sinan_hepatite_C$ID_PAIS_NE  <- as.character(sinan_hepatite_C$ID_PAIS_NE)
sinan_hepatite_C$ID_PAIS_WI  <- as.character(sinan_hepatite_C$ID_PAIS_WI)

sinan_hepatite_C$ID_UNIDADE  <- as.character(sinan_hepatite_C$ID_UNIDADE)
sinan_hepatite_C$ID_UNIDA_A  <- as.character(sinan_hepatite_C$ID_UNIDA_A)

sinan_hepatite_C$RACA_WIN  <- as.character(sinan_hepatite_C$RACA_WIN)
sinan_hepatite_C$CS_RACA_NE  <- as.character(sinan_hepatite_C$CS_RACA_NE)

sinan_hepatite_C$GESTANTE_W  <- as.character(sinan_hepatite_C$GESTANTE_W)
sinan_hepatite_C$CS_GESTANT  <- as.character(sinan_hepatite_C$CS_GESTANT)

sinan_hepatite_C$CS_ESCOL_N  <- as.character(sinan_hepatite_C$CS_ESCOL_N)
sinan_hepatite_C$ESCOLARIDA  <- as.character(sinan_hepatite_C$ESCOLARIDA)

sinan_hepatite_C$OCUPACIO_N  <- as.character(sinan_hepatite_C$OCUPACIO_N)
sinan_hepatite_C$OCUPACIO_W  <- as.character(sinan_hepatite_C$OCUPACIO_W)

sinan_hepatite_C$HEPATITA_N  <- as.character(sinan_hepatite_C$HEPATITA_N)
sinan_hepatite_C$HEPATITA_W  <- as.character(sinan_hepatite_C$HEPATITA_W)

sinan_hepatite_C$HEPATITB_N  <- as.character(sinan_hepatite_C$HEPATITB_N)
sinan_hepatite_C$HEPATITB_W  <- as.character(sinan_hepatite_C$HEPATITB_W)

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(ano_notificacao = coalesce(NU_ANO_WIN, NU_ANO_NET))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(dt_notificacao = coalesce(DT_NOTIFIC, DTNOT_WIN))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(fonte = coalesce(FONTE_NET, FONTE_WIN))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(forma = coalesce(FORMA_NET, FORMA_WIN))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(transplante = coalesce(TRANSPLA_N, TRANSPLA_W))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(hemodialise = coalesce(HEMODIALIS, HEMODIAL_A))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(tresoumais = coalesce(TRESMAIS_N, TRESMAIS_W))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(dentario = coalesce(DENTARIO_N, DENTARIO_W))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(cirurgico = coalesce(CIRURGICO, CIRURGIC_A))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(injetaveis = coalesce(INJETAVEIS, INJETAVE_A))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(transfusao = coalesce(TRANSFUSAO, TRANSFUS_A))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(acunputura = coalesce(ACUPUNTURA, ACUPUNTU_A))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(inal_crack = coalesce(INAL_CRACK, INALAVEIS))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(tatu_pierc = coalesce(TATU_PIER, TATUAGEM_W))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(sexual = coalesce(SEXUAL_NET, SEXUAL_WIN))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(idade = coalesce(NU_IDADE_N, IDADE_WIN))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(id_munic = coalesce(ID_MUNIC_A , ID_MUNICIP ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(id_pais = coalesce(ID_PAIS_NE , ID_PAIS_WI ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(id_unidade = coalesce(ID_UNIDADE , ID_UNIDA_A ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(raca = coalesce(RACA_WIN , CS_RACA_NE ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(gestante = coalesce(GESTANTE_W , CS_GESTANT ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(escolaridade = coalesce(CS_ESCOL_N , ESCOLARIDA ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(ocupacio = coalesce(OCUPACIO_N , OCUPACIO_W ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(vacina_A = coalesce(HEPATITA_N , HEPATITA_W ))

sinan_hepatite_C <- sinan_hepatite_C %>% mutate(vacina_B = coalesce(HEPATITB_N , HEPATITB_W ))

data_notif <- select(sinan_hepatite_C, ano_notificacao)

data_notif2 <- select(sinan_hepatite_C, dt_notificacao)

fonte <- select(sinan_hepatite_C, fonte)

missmap(data_notif)
missmap(data_notif2)
missmap(fonte)

##### Painel - Informações do SINAN - Causa da infecção, Sexo, Faixa etária

#sinan_hepatite_C_sel1 <- select(sinan_hepatite_C, NOME, NOMEMAE, DTNASC, MUNRES, SEXO, ID_MUNICIP, ID_UNIDADE, NM_PACIENT, NU_IDADE_N, CS_SEXO_NE, CS_RACA_NE, CS_CNS_SUS, HIV_NET, OUTRA_DST, SEXUAL_NET, TATU_PIER, MATBIOLOGI, INAL_CRACK, ACUPUNTURA, TRANSFUSAO, INJETAVEIS, CIRURGICO, AGUA_ALIME, DENTARIO_N, TRESMAIS_N, HEMODIALIS, TRANSPLA_N, OUTRAS_NET, CLASSI_FIN,FORMA_NET,CLAS_ETIOL, DSFONTE_NE, SEXO_WIN, IDADE_WIN, RACA_WIN,FORMA_WIN, FONTE_WIN, ANTIHAVI_A, ALCOOLISMO, TRANSFUS_A, ACUPUNTU_A, HEMODIAL_A, EXPOSICAO, TATUAGEM_W, INJETAVE_A, INJETAVE_A, CIRURGIC_A, PIERCING_W, INALAVEIS, DENTARIO_W, TRANSPLA_W, ACIDPERC_W, TRESMAIS_W, PARTO_WIN, PARCEIROS, DST_WIN, SEXUAL_WIN, ANTIHAVIGG, ano_notificacao  )

#missmap(sinan_hepatite_C_sel1)

sinan_hepatite_C_sel2 <- select(sinan_hepatite_C, NOME, NOMEMAE, DTNASC, MUNRES, SEXO,id_pais, id_munic, id_unidade, NM_PACIENT, idade,gestante, raca, HIV_NET, OUTRAS_NET, sexual, tatu_pierc, MATBIOLOGI, inal_crack, acunputura, transfusao, injetaveis, cirurgico, AGUA_ALIME, dentario, tresoumais, hemodialise, transplante, OUTRAS_NET, CLASSI_FIN,forma,CLAS_ETIOL, ano_notificacao, dt_notificacao, fonte, escolaridade, ocupacio, vacina_A, vacina_B)

missmap(sinan_hepatite_C_sel2)

colnames(sinan_hepatite_C_sel2)

######
## Informações de mês
######

sinan_hepatite_C_sel2$mes <- as.numeric(format(sinan_hepatite_C_sel2$dt_notificacao,'%m'))

##########################################

###############
#### contagem e porcentagem
##############

sinan_hepatite_C_sel2 <- sinan_hepatite_C_sel2 %>% group_by(NOME, NOMEMAE, DTNASC, MUNRES, SEXO,id_pais, id_munic, id_unidade, NM_PACIENT, idade, gestante, raca, HIV_NET, OUTRAS_NET, sexual, tatu_pierc, MATBIOLOGI,inal_crack, acunputura, transfusao, injetaveis, cirurgico, AGUA_ALIME,dentario, tresoumais, hemodialise, transplante, CLASSI_FIN, forma, CLAS_ETIOL, ano_notificacao, dt_notificacao, fonte, escolaridade, ocupacio, vacina_A, vacina_B, mes   )
sinan_hepatite_C_sel2_n <- sinan_hepatite_C_sel2 %>% summarise(n = n())

### removendo duplicado

sinan_hepatite_C_sel2_n_duplicado <- filter(sinan_hepatite_C_sel2_n, n==2)

sinan_hepatite_C_sel2_un_n <- filter(sinan_hepatite_C_sel2_n, n!=2)

sinan_hepatite_C_sel2_un_n$n_pct = sinan_hepatite_C_sel2_un_n$n / sum(sinan_hepatite_C_sel2_un_n$n) 

##### Substituindo códigos por informações
sinan_hepatite_C_sel2_un_n$raca  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$raca )
sinan_hepatite_C_sel2_un_n$fonte  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$fonte )
sinan_hepatite_C_sel2_un_n$id_pais  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$id_pais )
sinan_hepatite_C_sel2_un_n$tatu_pierc  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$tatu_pierc )
sinan_hepatite_C_sel2_un_n$inal_crack  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$inal_crack )
sinan_hepatite_C_sel2_un_n$acunputura  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$acunputura )
sinan_hepatite_C_sel2_un_n$transfusao  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$transfusao)
sinan_hepatite_C_sel2_un_n$injetaveis  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$injetaveis)
sinan_hepatite_C_sel2_un_n$cirurgico  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$cirurgico)
sinan_hepatite_C_sel2_un_n$dentario  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$dentario)
sinan_hepatite_C_sel2_un_n$tresoumais  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$tresoumais)
sinan_hepatite_C_sel2_un_n$hemodialise  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$hemodialise)
sinan_hepatite_C_sel2_un_n$transplante  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$transplante)
sinan_hepatite_C_sel2_un_n$vacina_A  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$vacina_A)
sinan_hepatite_C_sel2_un_n$vacina_B  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$vacina_B)
sinan_hepatite_C_sel2_un_n$gestante  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$gestante)
sinan_hepatite_C_sel2_un_n$sexual  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$sexual)
sinan_hepatite_C_sel2_un_n$ocupacio  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$ocupacio)
sinan_hepatite_C_sel2_un_n$forma  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$forma)
sinan_hepatite_C_sel2_un_n$escolaridade  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$escolaridade)
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CLAS_ETIOL)
sinan_hepatite_C_sel2_un_n$CLASSI_FIN  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$CLASSI_FIN)
sinan_hepatite_C_sel2_un_n$HIV_NET  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$HIV_NET)
sinan_hepatite_C_sel2_un_n$OUTRAS_NET  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$OUTRAS_NET)
sinan_hepatite_C_sel2_un_n$MATBIOLOGI  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$MATBIOLOGI)
sinan_hepatite_C_sel2_un_n$AGUA_ALIME  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$AGUA_ALIME)
sinan_hepatite_C_sel2_un_n$idade  <- fct_explicit_na(sinan_hepatite_C_sel2_un_n$idade)

sinan_hepatite_C_sel2_un_n$raca <- as.character(sinan_hepatite_C_sel2_un_n$raca)

sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "1"] <- "Branca"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "2"] <- "Preta"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "3"] <- "Amarela"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "4"] <- "Parda"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "5"] <- "Indígena"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$raca[sinan_hepatite_C_sel2_un_n$raca == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$fonte <- as.character(sinan_hepatite_C_sel2_un_n$fonte)

sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "01"] <- "Sexual"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "02"] <- "Transfusional"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "03"] <- "Uso de Drogas"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "04"] <- "Vertical"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "05"] <- "Acidente de Trabalho"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "06"] <- "Hemodiálise"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "07"] <- "Domiciliar"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "08"] <- "Tratamento cirúrgico"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "09"] <- "Tratamento dentário"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "10"] <- "Pessoa/pessoa"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "11"] <- "Alimento/água"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "12"] <- "Outros"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "99"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$fonte[sinan_hepatite_C_sel2_un_n$fonte == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$id_pais <- as.character(sinan_hepatite_C_sel2_un_n$id_pais)

sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "001"] <- "Brasil"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "1"] <- "Brasil"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "016"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "022"] <- "Bolívia"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "033"] <- "Aruba"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "105"] <- "Holanda"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "107"] <- "Ilhas Baleares"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "114"] <- "Iugoslávia"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "126"] <- "Suécia"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "137"] <- "Montenegro"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "138"] <- "Azerbaijão"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "151"] <- "Kalmir"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "156"] <- "Lituânia"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "184"] <- "Ilhas Comores"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "199"] <- "Ilhas Canárias"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "21"] <- "Argentina"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "247"] <- "Catar"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "253"] <- "Hong Kong"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "261"] <- "Camboja"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "31"] <- "Bélgica"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "32"] <- "Reino Unido"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "40"] <- "Comunidade das Bahamas"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "68"] <- "Martinica"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "7"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "800"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$id_pais[sinan_hepatite_C_sel2_un_n$id_pais == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$tatu_pierc <- as.character(sinan_hepatite_C_sel2_un_n$tatu_pierc)

sinan_hepatite_C_sel2_un_n$tatu_pierc[sinan_hepatite_C_sel2_un_n$tatu_pierc == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$tatu_pierc[sinan_hepatite_C_sel2_un_n$tatu_pierc == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$tatu_pierc[sinan_hepatite_C_sel2_un_n$tatu_pierc == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$tatu_pierc[sinan_hepatite_C_sel2_un_n$tatu_pierc == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$tatu_pierc[sinan_hepatite_C_sel2_un_n$tatu_pierc == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$inal_crack <- as.character(sinan_hepatite_C_sel2_un_n$inal_crack)

sinan_hepatite_C_sel2_un_n$inal_crack[sinan_hepatite_C_sel2_un_n$inal_crack == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$inal_crack[sinan_hepatite_C_sel2_un_n$inal_crack == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$inal_crack[sinan_hepatite_C_sel2_un_n$inal_crack == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$inal_crack[sinan_hepatite_C_sel2_un_n$inal_crack == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$inal_crack[sinan_hepatite_C_sel2_un_n$inal_crack == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$acunputura <- as.character(sinan_hepatite_C_sel2_un_n$acunputura)

sinan_hepatite_C_sel2_un_n$acunputura[sinan_hepatite_C_sel2_un_n$acunputura == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$acunputura[sinan_hepatite_C_sel2_un_n$acunputura == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$acunputura[sinan_hepatite_C_sel2_un_n$acunputura == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$acunputura[sinan_hepatite_C_sel2_un_n$acunputura == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$acunputura[sinan_hepatite_C_sel2_un_n$acunputura == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$transfusao <- as.character(sinan_hepatite_C_sel2_un_n$transfusao)

sinan_hepatite_C_sel2_un_n$transfusao[sinan_hepatite_C_sel2_un_n$transfusao == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$transfusao[sinan_hepatite_C_sel2_un_n$transfusao == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$transfusao[sinan_hepatite_C_sel2_un_n$transfusao == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$transfusao[sinan_hepatite_C_sel2_un_n$transfusao == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$transfusao[sinan_hepatite_C_sel2_un_n$transfusao == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$injetaveis <- as.character(sinan_hepatite_C_sel2_un_n$injetaveis)

sinan_hepatite_C_sel2_un_n$injetaveis[sinan_hepatite_C_sel2_un_n$injetaveis == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$injetaveis[sinan_hepatite_C_sel2_un_n$injetaveis == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$injetaveis[sinan_hepatite_C_sel2_un_n$injetaveis == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$injetaveis[sinan_hepatite_C_sel2_un_n$injetaveis == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$injetaveis[sinan_hepatite_C_sel2_un_n$injetaveis == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$cirurgico <- as.character(sinan_hepatite_C_sel2_un_n$cirurgico)

sinan_hepatite_C_sel2_un_n$cirurgico[sinan_hepatite_C_sel2_un_n$cirurgico == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$cirurgico[sinan_hepatite_C_sel2_un_n$cirurgico == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$cirurgico[sinan_hepatite_C_sel2_un_n$cirurgico == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$cirurgico[sinan_hepatite_C_sel2_un_n$cirurgico == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$cirurgico[sinan_hepatite_C_sel2_un_n$cirurgico == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$dentario <- as.character(sinan_hepatite_C_sel2_un_n$dentario)

sinan_hepatite_C_sel2_un_n$dentario[sinan_hepatite_C_sel2_un_n$dentario == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$dentario[sinan_hepatite_C_sel2_un_n$dentario == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$dentario[sinan_hepatite_C_sel2_un_n$dentario == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$dentario[sinan_hepatite_C_sel2_un_n$dentario == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$dentario[sinan_hepatite_C_sel2_un_n$dentario == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$tresoumais <- as.character(sinan_hepatite_C_sel2_un_n$tresoumais)

sinan_hepatite_C_sel2_un_n$tresoumais[sinan_hepatite_C_sel2_un_n$tresoumais == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$tresoumais[sinan_hepatite_C_sel2_un_n$tresoumais == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$tresoumais[sinan_hepatite_C_sel2_un_n$tresoumais == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$tresoumais[sinan_hepatite_C_sel2_un_n$tresoumais == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$tresoumais[sinan_hepatite_C_sel2_un_n$tresoumais == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$hemodialise <- as.character(sinan_hepatite_C_sel2_un_n$hemodialise)

sinan_hepatite_C_sel2_un_n$hemodialise[sinan_hepatite_C_sel2_un_n$hemodialise == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$hemodialise[sinan_hepatite_C_sel2_un_n$hemodialise == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$hemodialise[sinan_hepatite_C_sel2_un_n$hemodialise == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$hemodialise[sinan_hepatite_C_sel2_un_n$hemodialise == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$hemodialise[sinan_hepatite_C_sel2_un_n$hemodialise == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$transplante <- as.character(sinan_hepatite_C_sel2_un_n$transplante)

sinan_hepatite_C_sel2_un_n$transplante[sinan_hepatite_C_sel2_un_n$transplante == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$transplante[sinan_hepatite_C_sel2_un_n$transplante == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$transplante[sinan_hepatite_C_sel2_un_n$transplante == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$transplante[sinan_hepatite_C_sel2_un_n$transplante == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$transplante[sinan_hepatite_C_sel2_un_n$transplante == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$vacina_A <- as.character(sinan_hepatite_C_sel2_un_n$vacina_A)

sinan_hepatite_C_sel2_un_n$vacina_A[sinan_hepatite_C_sel2_un_n$vacina_A == "1"] <- "Completa"
sinan_hepatite_C_sel2_un_n$vacina_A[sinan_hepatite_C_sel2_un_n$vacina_A == "2"] <- "Incompleta"
sinan_hepatite_C_sel2_un_n$vacina_A[sinan_hepatite_C_sel2_un_n$vacina_A == "3"] <- "Não vacinado"
sinan_hepatite_C_sel2_un_n$vacina_A[sinan_hepatite_C_sel2_un_n$vacina_A == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$vacina_A[sinan_hepatite_C_sel2_un_n$vacina_A == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$vacina_B <- as.character(sinan_hepatite_C_sel2_un_n$vacina_B)

sinan_hepatite_C_sel2_un_n$vacina_B[sinan_hepatite_C_sel2_un_n$vacina_B == "1"] <- "Completa"
sinan_hepatite_C_sel2_un_n$vacina_B[sinan_hepatite_C_sel2_un_n$vacina_B == "2"] <- "Incompleta"
sinan_hepatite_C_sel2_un_n$vacina_B[sinan_hepatite_C_sel2_un_n$vacina_B == "3"] <- "Não vacinado"
sinan_hepatite_C_sel2_un_n$vacina_B[sinan_hepatite_C_sel2_un_n$vacina_B == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$vacina_B[sinan_hepatite_C_sel2_un_n$vacina_B == "(Missing)" ] <- "Ignorado"

##### Substituição código IBGE município por nome do Município

mun_Brasil <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto4/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx")

mun_Brasil <- select(mun_Brasil, MUNRES = Código.Município.Completo, Nome_Município)

mun_Brasil$MUNRES = substr(mun_Brasil$MUNRES,1,nchar(mun_Brasil$MUNRES)-1)

sinan_hepatite_C_sel2_un_n$MUNRES <- as.character(sinan_hepatite_C_sel2_un_n$MUNRES)

sinan_hepatite_C_sel2_un_n <- inner_join(sinan_hepatite_C_sel2_un_n, mun_Brasil, by = "MUNRES" )

### UF a parir do município de residência

sinan_hepatite_C_sel2_un_n$UF <- substr(sinan_hepatite_C_sel2_un_n$MUNRES, 0, 2)

sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "11"] <- "Rondônia"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "12"] <- "Acre"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "13"] <- "Amazonas"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "14"] <- "Roraima"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "15"] <- "Pará"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "16"] <- "Amapá"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "17"] <- "Tocantins"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "21"] <- "Maranhão"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "22"] <- "Piauí"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "23"] <- "Ceará"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "24"] <- "Rio Grande do Norte"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "25"] <- "Paraíba"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "26"] <- "Pernambuco"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "27"] <- "Alagoas"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "28"] <- "Sergipe"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "29"] <- "Bahia"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "31"] <- "Minas Gerais"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "32"] <- "Espírito Santo"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "33"] <- "Rio de Janeiro"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "35"] <- "São Paulo"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "41"] <- "Paraná"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "42"] <- "Santa Catarina"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "43"] <- "Rio Grande do Sul"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "50"] <- "Mato Grosso do Sul"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "51"] <- "Mato Grosso"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "52"] <- "Goiás"
sinan_hepatite_C_sel2_un_n$UF[sinan_hepatite_C_sel2_un_n$UF == "53"] <- "Distrito Federal"

sinan_hepatite_C_sel2_un_n$gestante <- as.character(sinan_hepatite_C_sel2_un_n$gestante)

sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "1"] <- "1ºTrimestre"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "2"] <- "2ºTrimestre"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "3"] <- "3ºTrimestre"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "4"] <- "Idade gestacional Ignorada"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "5"] <- "Não"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "6"] <- "Não se aplica"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$gestante[sinan_hepatite_C_sel2_un_n$gestante == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$sexual <- as.character(sinan_hepatite_C_sel2_un_n$sexual)

sinan_hepatite_C_sel2_un_n$sexual[sinan_hepatite_C_sel2_un_n$sexual == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$sexual[sinan_hepatite_C_sel2_un_n$sexual == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$sexual[sinan_hepatite_C_sel2_un_n$sexual == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$sexual[sinan_hepatite_C_sel2_un_n$sexual == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$sexual[sinan_hepatite_C_sel2_un_n$sexual == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$ocupacio <- as.character(sinan_hepatite_C_sel2_un_n$ocupacio)

sinan_hepatite_C_sel2_un_n$ocupacio[sinan_hepatite_C_sel2_un_n$ocupacio == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$ocupacio[sinan_hepatite_C_sel2_un_n$ocupacio == "2"] <- "Sim há mais de 6 meses"
sinan_hepatite_C_sel2_un_n$ocupacio[sinan_hepatite_C_sel2_un_n$ocupacio == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$ocupacio[sinan_hepatite_C_sel2_un_n$ocupacio == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$ocupacio[sinan_hepatite_C_sel2_un_n$ocupacio == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$forma <- as.character(sinan_hepatite_C_sel2_un_n$forma)

sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "1"] <- "Hepatite Aguda"
sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "2"] <- "Hepatite Crônica/Portador Assintomático"
sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "3"] <- "Hepatite Fulminante"
sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "4"] <- "Inconclusivo"
sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$forma[sinan_hepatite_C_sel2_un_n$forma == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$escolaridade <- as.character(sinan_hepatite_C_sel2_un_n$escolaridade)

sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "0"] <- "Analfabeto"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "00"] <- "Analfabeto"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "1"] <- "1ª a 4ª série incompleta do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "01"] <- "1ª a 4ª série incompleta do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "2"] <- "4ª série completa do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "02"] <- "4ª série completa do EF (antigo primário ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "3"] <- "5ª à 8ª série incompleta do EF (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "03"] <- "5ª à 8ª série incompleta do EF (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "4"] <- "Ensino fundamental completo (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "04"] <- "Ensino fundamental completo (antigo ginásio ou 1º grau)"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "5"] <- "5-Ensino médio incompleto (antigo colegial ou 2º grau )"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "05"] <- "5-Ensino médio incompleto (antigo colegial ou 2º grau )"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "6"] <- "Ensino médio completo (antigo colegial ou 2º grau ) "
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "06"] <- "Ensino médio completo (antigo colegial ou 2º grau ) "
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "7"] <- "Educação superior incompleta"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "07"] <- "Educação superior incompleta"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "8"] <- "Educação superior completa"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "08"] <- "Educação superior completa"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "09"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "10"] <- " Não se aplica"
sinan_hepatite_C_sel2_un_n$escolaridade[sinan_hepatite_C_sel2_un_n$escolaridade == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CLAS_ETIOL <- as.character(sinan_hepatite_C_sel2_un_n$CLAS_ETIOL)

sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "01"] <- "Vírus A"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "02"] <- "Vírus B"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "03"] <- "Vírus C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "04"] <- "Vírus B e D"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "05"] <- "Vírus E"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "06"] <- "Vírus B e C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "07"] <- "Vírus A e B"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "08"] <- "Vírus A e C"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "09"] <- "Outras hepatites virais"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "99"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$CLAS_ETIOL[sinan_hepatite_C_sel2_un_n$CLAS_ETIOL == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$CLASSI_FIN <- as.character(sinan_hepatite_C_sel2_un_n$CLASSI_FIN)

sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "1"] <- "Confirmação laboratorial"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "2"] <- "Confirmação clínico-epidemiológica"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "3"] <- "Descartado"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "4"] <- "Cicatriz sorológica"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "8"] <- "Inconclusivo"
sinan_hepatite_C_sel2_un_n$CLASSI_FIN[sinan_hepatite_C_sel2_un_n$CLASSI_FIN == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$HIV_NET <- as.character(sinan_hepatite_C_sel2_un_n$HIV_NET)

sinan_hepatite_C_sel2_un_n$HIV_NET[sinan_hepatite_C_sel2_un_n$HIV_NET == "1"] <- "Sim"
sinan_hepatite_C_sel2_un_n$HIV_NET[sinan_hepatite_C_sel2_un_n$HIV_NET == "2"] <- "Não"
sinan_hepatite_C_sel2_un_n$HIV_NET[sinan_hepatite_C_sel2_un_n$HIV_NET == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$HIV_NET[sinan_hepatite_C_sel2_un_n$HIV_NET == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$OUTRAS_NET <- as.character(sinan_hepatite_C_sel2_un_n$OUTRAS_NET)

sinan_hepatite_C_sel2_un_n$OUTRAS_NET[sinan_hepatite_C_sel2_un_n$OUTRAS_NET == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$OUTRAS_NET[sinan_hepatite_C_sel2_un_n$OUTRAS_NET == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$OUTRAS_NET[sinan_hepatite_C_sel2_un_n$OUTRAS_NET == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$OUTRAS_NET[sinan_hepatite_C_sel2_un_n$OUTRAS_NET == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$OUTRAS_NET[sinan_hepatite_C_sel2_un_n$OUTRAS_NET == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$MATBIOLOGI <- as.character(sinan_hepatite_C_sel2_un_n$MATBIOLOGI)

sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$MATBIOLOGI[sinan_hepatite_C_sel2_un_n$MATBIOLOGI == "(Missing)" ] <- "Ignorado"

sinan_hepatite_C_sel2_un_n$AGUA_ALIME <- as.character(sinan_hepatite_C_sel2_un_n$AGUA_ALIME)

sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "1"] <- "Sim há menos de 6 meses"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "2"] <- "Sim há mais deseis meses"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "3"] <- "Não"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "9"] <- "Ignorado"
sinan_hepatite_C_sel2_un_n$AGUA_ALIME[sinan_hepatite_C_sel2_un_n$AGUA_ALIME == "(Missing)" ] <- "Ignorado"

##### Cálculo de faixa etária por idade
sinan_hepatite_C_sel2_un_n$idade <- as.character(sinan_hepatite_C_sel2_un_n$idade)

sinan_hepatite_C_sel2_un_n$idade = substr(sinan_hepatite_C_sel2_un_n$idade,3,nchar(sinan_hepatite_C_sel2_un_n$idade)-0)

sinan_hepatite_C_sel2_un_n$idade <- as.numeric(sinan_hepatite_C_sel2_un_n$idade)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

sinan_hepatite_C_sel2_un_n$IDADE_faixa <- cut(sinan_hepatite_C_sel2_un_n$idade, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

# remover coluna NOME e NOMEMAE
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]

sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]
sinan_hepatite_C_sel2_un_n <- sinan_hepatite_C_sel2_un_n[,-1]

sinan_hepatite_C_sel2_un_n <- select(sinan_hepatite_C_sel2_un_n, -NM_PACIENT)

sinan_hepatite_C_sel2_un_n$n_pct <- round(sinan_hepatite_C_sel2_un_n$n_pct * 100, digits = 4)

write.csv(sinan_hepatite_C_sel2_un_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_1999_2019.csv")

###############
##### Unir dados de 1999-2019 aos dados de 2020

sinan_hepatite_C_sel2_un_n_2020 <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_2020.xlsx")

sinan_hepatite_C_sel2_un_n_1999_2019 <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_1999_2019.csv")

sinan_hepatite_C_sel2_un_n_1999_2019 <- select(sinan_hepatite_C_sel2_un_n_1999_2019, -X, -NM_PACIENT)

sinan_hepatite_C_sel2_un_n_2020 <- select(sinan_hepatite_C_sel2_un_n_2020, -HEPATITE_N, -OUTRAS)

sinan_hepatite_C_sel2_un_n_1999_2019 <- select(sinan_hepatite_C_sel2_un_n_1999_2019, ID_MUNICIP = id_munic, CS_SEXO = SEXO, ID_PAIS = id_pais, ID_UNIDADE = id_unidade, NU_IDADE_N = idade, CS_GESTANT = gestante, CS_RACA = raca, HIV = HIV_NET ,OUTRA_DST = OUTRAS_NET, SEXUAL = sexual, TATU_PIER = tatu_pierc, MATBIOLOGI, INAL_CRACK = inal_crack,  ACUPUNTURA = acunputura, TRANSFUSAO = transfusao, INJETAVEIS = injetaveis, CIRURGICO = cirurgico, AGUA_ALIME, DENTARIO = dentario, TRESMAIS = tresoumais, HEMODIALIS = hemodialise, TRANSPLA = transplante, CLASSI_FIN, FORMA = forma, CLAS_ETIOL = CLAS_ETIOL, NU_ANO = ano_notificacao, DT_NOTIFIC = dt_notificacao,FONTE = fonte, CS_ESCOL_N = escolaridade, OCUPACIO = ocupacio, HEPATITA = vacina_A, HEPATITB = vacina_B, mes, n, n_pct, Nome_Município, UF, IDADE_faixa )

sinan_hepatite_C_sel2_un_n_1999_2020 <- do.call("rbind", list(sinan_hepatite_C_sel2_un_n_1999_2019,  sinan_hepatite_C_sel2_un_n_2020))

sinan_hepatite_C_sel2_un_n_1999_2020 <- select(sinan_hepatite_C_sel2_un_n_1999_2020, -n_pct)

sinan_hepatite_C_sel2_un_n_1999_2020 = sinan_hepatite_C_sel2_un_n_1999_2020[-c(191422),]

write.csv(sinan_hepatite_C_sel2_un_n_1999_2020, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/BANCOS_TABELAS/sinan_hepatite_C_sel2_un_n_1999_2020.csv")


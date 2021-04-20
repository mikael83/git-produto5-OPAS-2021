######################################################################
############            Script GAL - Hepatite D      #################
########### V.1.0 - Desenvolvido por Mikael Lemos ####################
######################################################################

#### Carregando bibliotecas ####

#install.packages('gtools')
library(gtools)

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

#library(xlsx)

library(rJava)

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

#install.packages("forcats")
library(forcats)

library(readr)

#install.packages("fs")

library(fs)

#install.packages("stringi")
library(stringi)

#install.packages("esquisse")
library(esquisse)

library(data.table)
#install.packages("readxl")
library(readxl)

#install.packages("writexl")
library(writexl)

setwd("C:/Users/lemos/Downloads/HEPATITES/BANCOS/GAL2018_hepc_completo/n0/n0_x/")
setwd("C:/Users/lemos/Downloads/HEPATITES/BANCOS/GAL2019_hepc_completo/n0/n0_x/")
setwd("C:/Users/lemos/Downloads/HEPATITES/BANCOS/GAL2020_hepc_completo/n0/n0_x/")

# Carregar arquivos 

# GAL 2018
file_names <- dir("./") #where you have your files

hep_C_GAL_2018 <- do.call(smartbind,lapply(file_names,read.csv,head = TRUE, sep=";" ))

# GAL 2019
hep_C_GAL_2019 <- do.call(smartbind,lapply(file_names,read.csv,head = TRUE, sep=";" ))

# GAL 2020
hep_C_GAL_2020 <- do.call(smartbind,lapply(file_names,read.csv,head = TRUE, sep=";" ))

write.csv(hep_C_GAL_2018, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2018.csv")
write.csv(hep_C_GAL_2019, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2019.csv")
write.csv(hep_C_GAL_2020, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2020.csv")

hep_C_GAL_2018 <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2018.csv")
hep_C_GAL_2019 <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2019.csv")
hep_C_GAL_2020 <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/hep_C_GAL_2020.csv")

colnames(hep_C_GAL_2018)
colnames(hep_C_GAL_2019)
colnames(hep_C_GAL_2020)

hep_C_GAL_2018 <- select(hep_C_GAL_2018,-X, "GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")
hep_C_GAL_2018 <- select(hep_C_GAL_2018, "GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")
hep_C_GAL_2019 <- select(hep_C_GAL_2019, -X, "GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")
hep_C_GAL_2019 <- select(hep_C_GAL_2019,"GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")
hep_C_GAL_2020 <- select(hep_C_GAL_2020, -X, "GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")
hep_C_GAL_2020 <- select(hep_C_GAL_2020,"GAL.Emissor", "Laboratório.de.Cadastro","Unidade.Requisitante", "Municipio.do.Requisitante", "Data.de.Nascimento","Tipo.Idade","Raça.Cor","Municipio.de.Residência" , "Estado.de.Residência","Amostra", "Data.da.Coleta", "Medicamento" , "Kit", "Lote.do.Kit", "CNES.Laboratório.Responsável", "CNES.Laboratório.Executor", "Status.Exame", "Valor", "Cut.Off", "Estado.da.Requisição",  "CNES.Laboratório.de.Cadastro", "CNES.Unidade.Solicitante", "IBGE.Município.Solicitante" , "CNS.do.Profissional.de.Saúde", "Reg..Conselho.Matrícula", "Idade","Sexo", "Nacionalidade", "IBGE.Município.de.Residência" ,"País.de.Residência","Exame", "Exame.Correlativo..S.N.", "Material.Biológico", "Material.Clínico", "Fabricante", "Reteste", "Laboratório.Responsável", "Laboratório.Executor", "Resultado")

hep_C_GAL_2018$ano <- "2018"
hep_C_GAL_2019$ano <- "2019"
hep_C_GAL_2020$ano <- "2020"

hepC_GAL_2018_2020 <- do.call("rbind", list( hep_C_GAL_2018,hep_C_GAL_2019, hep_C_GAL_2020))
write.csv(hepC_GAL_2018_2020, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto4/BANCOS/hepC_GAL_2018_2020.csv")

hepC_GAL_2018_2020 <- hepC_GAL_2018_2020 %>% group_by(GAL.Emissor, Laboratório.de.Cadastro,Unidade.Requisitante, Municipio.do.Requisitante, Data.de.Nascimento,Tipo.Idade,Raça.Cor,Municipio.de.Residência, Estado.de.Residência,Amostra, Data.da.Coleta, Medicamento , Kit, Lote.do.Kit, CNES.Laboratório.Responsável, CNES.Laboratório.Executor, Status.Exame, Valor, Cut.Off, Estado.da.Requisição, CNES.Laboratório.de.Cadastro, CNES.Unidade.Solicitante, IBGE.Município.Solicitante , CNS.do.Profissional.de.Saúde, Reg..Conselho.Matrícula, Idade,Sexo, Nacionalidade, IBGE.Município.de.Residência, País.de.Residência, Exame, Exame.Correlativo..S.N., Material.Biológico, Material.Clínico, Fabricante, Reteste, Laboratório.Responsável, Laboratório.Executor, Resultado, ano)
hepC_GAL_2018_2020_n <- hepC_GAL_2018_2020 %>% summarise(n = n())

hepC_GAL_2018_2020_n_un <- filter(hepC_GAL_2018_2020_n, n<2)

hepC_GAL_2018_2020_n <- filter(hepC_GAL_2018_2020_n, n<2)

##### Cálculo de faixa etária por idade

hepC_GAL_2018_2020_n$Idade <- as.numeric(hepC_GAL_2018_2020_n$Idade)

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

hepC_GAL_2018_2020_n$IDADE_faixa <- cut(hepC_GAL_2018_2020_n$Idade, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

## Porcentagem

hepC_GAL_2018_2020_n$n_pct = hepC_GAL_2018_2020_n$n / sum(hepC_GAL_2018_2020_n$n) 

missmap(hepC_GAL_2018_2020_n)

hepC_GAL_2018_2020_n <- select(hepC_GAL_2018_2020_n, -Reteste, -Cut.Off, -Valor , -Lote.do.Kit, -CNS.do.Profissional.de.Saúde, -Resultado, -Medicamento, -Fabricante, -Kit, -Data.de.Nascimento)

colnames(hepC_GAL_2018_2020_n)

## Preenchimento de ignorados/"missing" 
is.na(hepC_GAL_2018_2020_n) <- hepC_GAL_2018_2020_n==''
table(hepC_GAL_2018_2020_n$Raça.Cor)
hepC_GAL_2018_2020_n$Raça.Cor  <- fct_explicit_na(hepC_GAL_2018_2020_n$Raça.Cor)
hepC_GAL_2018_2020_n$Raça.Cor <- as.character(hepC_GAL_2018_2020_n$Raça.Cor)
hepC_GAL_2018_2020_n$Raça.Cor[hepC_GAL_2018_2020_n$Raça.Cor == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Nacionalidade)
hepC_GAL_2018_2020_n$Nacionalidade  <- fct_explicit_na(hepC_GAL_2018_2020_n$Nacionalidade)
hepC_GAL_2018_2020_n$Nacionalidade <- as.character(hepC_GAL_2018_2020_n$Nacionalidade)
hepC_GAL_2018_2020_n$Nacionalidade[hepC_GAL_2018_2020_n$Nacionalidade == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Material.Clínico)
hepC_GAL_2018_2020_n$Material.Clínico  <- fct_explicit_na(hepC_GAL_2018_2020_n$Material.Clínico)
hepC_GAL_2018_2020_n$Material.Clínico <- as.character(hepC_GAL_2018_2020_n$Material.Clínico)
hepC_GAL_2018_2020_n$Material.Clínico[hepC_GAL_2018_2020_n$Material.Clínico == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Resultado)
hepC_GAL_2018_2020_n$Resultado  <- fct_explicit_na(hepC_GAL_2018_2020_n$Resultado)
hepC_GAL_2018_2020_n$Resultado <- as.character(hepC_GAL_2018_2020_n$Resultado)
hepC_GAL_2018_2020_n$Resultado[hepC_GAL_2018_2020_n$Resultado == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$País.de.Residência)
hepC_GAL_2018_2020_n$País.de.Residência  <- fct_explicit_na(hepC_GAL_2018_2020_n$País.de.Residência)
hepC_GAL_2018_2020_n$País.de.Residência <- as.character(hepC_GAL_2018_2020_n$País.de.Residência)
hepC_GAL_2018_2020_n$País.de.Residência[hepC_GAL_2018_2020_n$País.de.Residência == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Material.Biológico)
hepC_GAL_2018_2020_n$Material.Biológico  <- fct_explicit_na(hepC_GAL_2018_2020_n$Material.Biológico)
hepC_GAL_2018_2020_n$Material.Biológico <- as.character(hepC_GAL_2018_2020_n$Material.Biológico)
hepC_GAL_2018_2020_n$Material.Biológico[hepC_GAL_2018_2020_n$Material.Biológico == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Exame.Correlativo..S.N.)
hepC_GAL_2018_2020_n$Exame.Correlativo..S.N.  <- fct_explicit_na(hepC_GAL_2018_2020_n$Exame.Correlativo..S.N.)
hepC_GAL_2018_2020_n$Exame.Correlativo..S.N. <- as.character(hepC_GAL_2018_2020_n$Exame.Correlativo..S.N.)
hepC_GAL_2018_2020_n$Exame.Correlativo..S.N.[hepC_GAL_2018_2020_n$Exame.Correlativo..S.N. == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Sexo)
hepC_GAL_2018_2020_n$Sexo  <- fct_explicit_na(hepC_GAL_2018_2020_n$Sexo)
hepC_GAL_2018_2020_n$Sexo <- as.character(hepC_GAL_2018_2020_n$Sexo)
hepC_GAL_2018_2020_n$Sexo[hepC_GAL_2018_2020_n$Sexo == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Status.Exame)
hepC_GAL_2018_2020_n$Status.Exame  <- fct_explicit_na(hepC_GAL_2018_2020_n$Status.Exame)
hepC_GAL_2018_2020_n$Status.Exame <- as.character(hepC_GAL_2018_2020_n$Status.Exame)
hepC_GAL_2018_2020_n$Status.Exame[hepC_GAL_2018_2020_n$Status.Exame == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula)
hepC_GAL_2018_2020_n$Status.Exame  <- fct_explicit_na(hepC_GAL_2018_2020_n$Status.Exame)
hepC_GAL_2018_2020_n$Status.Exame <- as.character(hepC_GAL_2018_2020_n$Status.Exame)
hepC_GAL_2018_2020_n$Status.Exame[hepC_GAL_2018_2020_n$Status.Exame == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula)
hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula  <- fct_explicit_na(hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula)
hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula <- as.character(hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula)
hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula[hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula == "(Missing)" ] <- "Ignorado"

table(hepC_GAL_2018_2020_n$Exame)
hepC_GAL_2018_2020_n$Exame  <- fct_explicit_na(hepC_GAL_2018_2020_n$Exame)
hepC_GAL_2018_2020_n$Exame <- as.character(hepC_GAL_2018_2020_n$Exame)
hepC_GAL_2018_2020_n$Exame[hepC_GAL_2018_2020_n$Exame == "(Missing)" ] <- "Ignorado"


write.xlsx( hepC_GAL_2018_2020_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/hepC_GAL_2018_2020_n_un.xlsx" )

write.csv( hepC_GAL_2018_2020_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/hepC_GAL_2018_2020_n_un.csv" )

##### Substituição código IBGE município por nome do Município

#mun_Brasil <- read.xlsx("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto4/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx")

#mun_Brasil <- select(mun_Brasil, MUNRES = Código.Município.Completo, Nome_Município)

#mun_Brasil$MUNRES = substr(mun_Brasil$MUNRES,1,nchar(mun_Brasil$MUNRES)-1)



### Lat e Long

colnames(hepC_GAL_2018_2020_n)[17] <- "Nome_Município"

#BR_localidades_selec_un$Nome_Município = toupper(BR_localidades_selec_un$Nome_Município)

hepC_GAL_2018_2020_n_lat_log <- inner_join(hepC_GAL_2018_2020_n, BR_localidades_selec_un,by = "Nome_Município" )

### Tabela reg conselho

reg_conselho <- hepC_GAL_2018_2020_n_lat_log %>% group_by(Reg..Conselho.Matrícula, Estado.de.Residência, Nome_Município, Resultado, CNES.Unidade.Solicitante, ano, LAT, LONG)
reg_conselho <- reg_conselho %>% summarise(n = n())

#reg_conselho <- table(hepC_GAL_2018_2020_n$Reg..Conselho.Matrícula)

#reg_conselho <- as.data.frame(reg_conselho)

reg_conselho_coren <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CORE")
reg_conselho_enf <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "ENF")
reg_conselho_CONRE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CONRE")
reg_conselho_COEREN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "COERE")
reg_conselho_CORN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CORN")
reg_conselho_CRN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CRN")

hepC_GAL_COREN <- do.call("rbind", list( reg_conselho_coren,reg_conselho_enf, reg_conselho_CONRE, reg_conselho_COEREN,reg_conselho_CORN, reg_conselho_CRN))
hepC_GAL_COREN$Reg..Conselho.Matrícula <- "ENFERMEIRO(A)"

reg_conselho_CRM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CRM")
reg_conselho_medico <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "MEDICO")
reg_conselho_gista <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "GISTA")
reg_conselho_medic <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "MEDIC")
reg_conselho_ccrm <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CCRM")
reg_conselho_CM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CM")
reg_conselho_CEM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CEM")
reg_conselho_CROM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CROM")

hepC_GAL_CRM <- do.call("rbind", list( reg_conselho_CRM,reg_conselho_medico, reg_conselho_gista, reg_conselho_medic,reg_conselho_ccrm, reg_conselho_CM, reg_conselho_CEM, reg_conselho_CROM))
hepC_GAL_CRM$Reg..Conselho.Matrícula <- "MÉDICO(A)"

reg_conselho_CRF <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CRF")
reg_conselho_CRF$Reg..Conselho.Matrícula <- "FARMACÊUTICO(A)"

reg_conselho_RMS <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "RMS")
reg_conselho_RMS$Reg..Conselho.Matrícula <- "MÉDICO(A) INTERCAMBISTA"

reg_conselho_CRBM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CRBM")
reg_conselho_CRBM$Reg..Conselho.Matrícula <- "BIOMÉDICO(A)"

##### Registros incorretos !!! ## ' ; - ; * ; ? ; 0 ; 00 ; 1 ; 2 ; 3 ; 4 ; 5 ;6 ; 7 ; 8 ; 9; ACRE ; CCIH ; CELIA ; CXXXXXX ; II ; ILEGIVEL , LILIEM , MAT , MERCEDES, MG31335, N/INF, NAO CADASTRADO, NAO IDENTIFICADO,NAO INFORMADO, NI, NIF, NNNN, OOOO, RO, VE , X , XX, ZX,  ZXXX

reg_conselho_err1 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^'")
reg_conselho_err1$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CEPEM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "CEPEM")
reg_conselho_CEPEM$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_VIG <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "VIG")
reg_conselho_VIG$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_LABORATORIO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "LABORATORIO")
reg_conselho_LABORATORIO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_0 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^0")
reg_conselho_0$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_1 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^1")
reg_conselho_1$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_2 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^2")
reg_conselho_2$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_3 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^3")
reg_conselho_3$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_4 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^4")
reg_conselho_4$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_5 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^5")
reg_conselho_5$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_6 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^6")
reg_conselho_6$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_7 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^7")
reg_conselho_7$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_8 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^8")
reg_conselho_8$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_9 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^9")
reg_conselho_9$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ACRE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ACRE")
reg_conselho_ACRE$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CCIH <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CCIH")
reg_conselho_CCIH$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CELIA <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CELIA")
reg_conselho_CELIA$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CXXXXXX <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CELIA")
reg_conselho_CXXXXXX$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_II <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^II")
reg_conselho_II$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ILEGIVEL <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ILEGIVEL")
reg_conselho_ILEGIVEL$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MERCEDES <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MERCEDES")
reg_conselho_MERCEDES$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MG31335 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MG31335")
reg_conselho_MG31335$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_NAO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NAO")
reg_conselho_NAO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_NAO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NAO")
reg_conselho_NAO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_NI <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NI")
reg_conselho_NI$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_NIF <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NIF")
reg_conselho_NIF$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_NNNN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NNNN")
reg_conselho_NNNN$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_OOOO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^OOOO")
reg_conselho_OOOO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_RO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^RO")
reg_conselho_RO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_VE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^VE")
reg_conselho_VE$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_X <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^X")
reg_conselho_X$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_Z <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^Z")
reg_conselho_Z$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho__ <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^-")
reg_conselho__$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_asterisco <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^\\*")
reg_conselho_asterisco$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_mais <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^\\+")
reg_conselho_mais$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CREM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CREM")
reg_conselho_CREM$Reg..Conselho.Matrícula <- "ENFERMEIRO(A)"

reg_conselho_CRED <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRED")
reg_conselho_CRED$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CRD <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRD")
reg_conselho_CRD$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CRN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRN")
reg_conselho_CRN$Reg..Conselho.Matrícula <- "NUTRICIONISTA"

reg_conselho_CRO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRO")
reg_conselho_CRO$Reg..Conselho.Matrícula <- "DENTISTA"

reg_conselho_CRP <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRP")
reg_conselho_CRP$Reg..Conselho.Matrícula <- "PSICÓLOGO(A)"

reg_conselho_CRRN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRRN")
reg_conselho_CRRN$Reg..Conselho.Matrícula <- "NUTRICIONISTA"

reg_conselho_CRRM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CRRM")
reg_conselho_CRRM$Reg..Conselho.Matrícula <- "MÉDICO(A)"

reg_conselho_CTM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CTM")
reg_conselho_CTM$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CTP <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CTP")
reg_conselho_CTP$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_CVOREN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CVOREN")
reg_conselho_CVOREN$Reg..Conselho.Matrícula <- "ENFERMEIRO(A)"

reg_conselho_CVRM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^CVRM")
reg_conselho_CVRM$Reg..Conselho.Matrícula <- "MÉDICO(A)"

reg_conselho_D <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^D")
reg_conselho_D$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ESTAGIARIA <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ESTAGIARIA")
reg_conselho_ESTAGIARIA$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_FARMA <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^FARMA")
reg_conselho_FARMA$Reg..Conselho.Matrícula <- "FARMAÊUTICO(A)"

reg_conselho_FG <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^FG")
reg_conselho_FG$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_FRANKLIN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like%  "^FRANKLIN"  )
reg_conselho_FRANKLIN$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_G <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^G"  )
reg_conselho_G$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_H <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^H"  )
reg_conselho_H$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_II <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^II"  )
reg_conselho_II$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_IN <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^IN" )
reg_conselho_IN$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ILE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ILE"  )
reg_conselho_ILE$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_INLE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^INLE"  )
reg_conselho_INLE$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_INEL <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^INEL"  )
reg_conselho_INEL$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_INFECTO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^INFECTO" )
reg_conselho_INFECTO$Reg..Conselho.Matrícula <- "MÉDICO(A)"

reg_conselho_INTERNA <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^INTERNA" )
reg_conselho_INTERNA$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_IGNORADO <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^IGNORADO" )
reg_conselho_IGNORADO$Reg..Conselho.Matrícula <- "IGNORADO"

reg_conselho_IG <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^IG" )
reg_conselho_IG$Reg..Conselho.Matrícula <- "IGNORADO"

reg_conselho_ignorado <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ignorado" )
reg_conselho_ignorado$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ISAB <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ISAB" )
reg_conselho_ISAB$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_JE <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^JE" )
reg_conselho_JE$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_JU <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^JU"  )
reg_conselho_JU$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MA <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MA" )
reg_conselho_MA$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_L <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^L" )
reg_conselho_L$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_M0 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^M0" )
reg_conselho_MO$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_M4 <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^M4"  )
reg_conselho_M4$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MED <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MED")
reg_conselho_MED$Reg..Conselho.Matrícula <- "MÉDICO(A)"

reg_conselho_MG <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^NG")
reg_conselho_MG$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MH <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MH" )
reg_conselho_MH$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MI <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MI"  )
reg_conselho_MI$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_ML <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^ML"  )
reg_conselho_ML$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_MS <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^MS"  )
reg_conselho_MS$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_N <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^N" )
reg_conselho_N$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_PRES <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^PRES" )
reg_conselho_PRES$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_PT <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^PT" )
reg_conselho_PT$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_RJ <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^RJ" )
reg_conselho_RJ$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_RM <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^RM"  )
reg_conselho_RM$Reg..Conselho.Matrícula <- "MÉDICO INTERCAMBISTA"

reg_conselho_S <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^S"  )
reg_conselho_S$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

reg_conselho_TEC <- reg_conselho %>% dplyr::filter(Reg..Conselho.Matrícula %like% "^TEC" )
reg_conselho_TEC$Reg..Conselho.Matrícula <- "ERRO DE PREENCHIMENTO"

hepC_cons_reg_mapa <- do.call("rbind", list(hepC_GAL_COREN, hepC_GAL_CRM, reg_conselho_CRF, reg_conselho_RMS,reg_conselho_CRBM, reg_conselho_err1, reg_conselho_CEPEM, reg_conselho_VIG, reg_conselho_LABORATORIO, reg_conselho_0,reg_conselho_1, reg_conselho_2, reg_conselho_3, reg_conselho_4, reg_conselho_5, reg_conselho_6, reg_conselho_7, reg_conselho_8, reg_conselho_9, reg_conselho_ACRE, reg_conselho_CCIH,  reg_conselho_CELIA,reg_conselho_CXXXXXX, reg_conselho_II,reg_conselho_ILEGIVEL, reg_conselho_MERCEDES, reg_conselho_MG31335, reg_conselho_NAO, reg_conselho_NI,  reg_conselho_NIF, reg_conselho_NNNN, reg_conselho_OOOO, reg_conselho_RO, reg_conselho_VE, reg_conselho_X, reg_conselho_Z, reg_conselho__, reg_conselho_asterisco, reg_conselho_mais, reg_conselho_CREM, reg_conselho_CRED, reg_conselho_CRD, reg_conselho_CRN, reg_conselho_CRO, reg_conselho_CRP, reg_conselho_CRRN, reg_conselho_CRRM, reg_conselho_CTM, reg_conselho_CTP, reg_conselho_CVOREN, reg_conselho_CVRM, reg_conselho_D, reg_conselho_ESTAGIARIA, reg_conselho_FARMA, reg_conselho_FG, reg_conselho_FRANKLIN, reg_conselho_G,reg_conselho_H, reg_conselho_II, reg_conselho_IN, reg_conselho_ILE, reg_conselho_INLE, reg_conselho_INEL, reg_conselho_INFECTO, reg_conselho_INTERNA, reg_conselho_IGNORADO, reg_conselho_IG, reg_conselho_ignorado, reg_conselho_ISAB, reg_conselho_JE, reg_conselho_JU, reg_conselho_MA, reg_conselho_L, reg_conselho_M0, reg_conselho_M4,reg_conselho_MED, reg_conselho_MG, reg_conselho_MH, reg_conselho_MI, reg_conselho_ML, reg_conselho_MS, reg_conselho_N, reg_conselho_PRES,reg_conselho_PT, reg_conselho_RJ,reg_conselho_RM,reg_conselho_S,reg_conselho_TEC  ))

write_xlsx(hepC_cons_reg_mapa, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2020/produto5/painel_hepC/hepC_cons_reg_mapa.xlsx")

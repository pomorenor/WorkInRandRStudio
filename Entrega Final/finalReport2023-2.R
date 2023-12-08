setwd("C:/Users/pohlj/OneDrive/Escritorio/Job/WorkInRandRStudio/Entrega Final")


library("dplyr")
library("readxl")
library("writexl")
library("stringdist")
library("openxlsx")


#Raw data contains all the data related to the lectures and professors, hour per undergrad...

ausentismo <- read_xlsx("AUSENTISMO A 30 NOVIEMBRE 2023.xlsx")
cargos <- read_xlsx("CARGOS ACADEMICOS ADMINISTRATIVOS ACTUALES A 07 DIC 2023 (GENERADOS DE SARA) (1).xlsx")
rawData <- read_xlsx("RE_HRO_GRU_PER.xlsx")
planta_docente_toworkwith <- read_xlsx("PLANTA DE CARGOS DOCENTE A 30 NOV 2023.xlsx")

# This comes after for filling the excel
#planta_docente <- loadWorkbook("PLANTA DE CARGOS DOCENTE A 30 NOV 2023.xlsx")


#First we are gonna deal with situaciones academicas and cargos following the rules we have established

ausentismo$FECHADOC <- as.Date(ausentismo$FECHADOC, format = "%d/%m/%Y")
ausentismo$`FECHA INICIO` <- as.Date(ausentismo$`FECHA INICIO`, format = "%d/%m/%Y")
ausentismo$FREGRESO <- as.Date(ausentismo$FREGRESO, format = "%d/%m/%Y")
ausentismo$`FECHA FINAL` <- as.Date(ausentismo$`FECHA FINAL`, format = "%d/%m/%Y")

fechaDeCorte <- as.Date("30/11/2023", format = "%d/%m/%Y")
fechaspreviasconsideradas <- c()

for(i in 30:37){
 fechaspreviasconsideradas <- c(fechaspreviasconsideradas, as.character(fechaDeCorte - i))
}

fechaspreviasconsideradas <- as.Date(fechaspreviasconsideradas)

profsConSituacion <- ausentismo %>%
  group_by(APELLIDOSYNOMBRES) %>%
  filter(`FECHA FINAL` == max(`FECHA FINAL`) & `FECHA FINAL` > fechaDeCorte | `FECHA FINAL`%in% fechaspreviasconsideradas)


planta_docente_toworkwith <- data.frame(planta_docente_toworkwith)

colnames(planta_docente_toworkwith)





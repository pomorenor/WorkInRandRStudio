setwd("C:/Users/pohlj/OneDrive/Escritorio/Job/WorkInRandRStudio/Entrega Final")

  
library("dplyr")
library("readxl")
library("writexl")
library("stringdist")
library("gtools")
library("matrixStats")
  
#Raw data contains all the data related to the lectures and professors, hour per undergrad...
  
ausentismo <- read_xlsx("AUSENTISMO A 30 NOVIEMBRE 2023.xlsx")
cargos <- read_xlsx("CARGOS ACADEMICOS ADMINISTRATIVOS ACTUALES A 07 DIC 2023 (GENERADOS DE SARA) (1).xlsx")
rawData <- read_xlsx("RE_HRO_GRU_PER_towork.xlsx")
planta_docente_toworkwith <- read_xlsx("PLANTA DE CARGOS DOCENTE A 30 NOV 2023.xlsx")
copy_of_planta_docente <- read_xlsx("PLANTA DE CARGOS DOCENTE A 30 NOV 2023.xlsx")

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
 # filter(`FECHA FINAL` == max(`FECHA FINAL`) | `FECHA FINAL` %in% fechaspreviasconsideradas)
  filter(`FECHA FINAL` == max(`FECHA FINAL`))
  
  
profsConSituacion <- profsConSituacion %>%
  group_by(APELLIDOSYNOMBRES) %>%
  filter(`FECHA FINAL` >= fechaDeCorte)
  
Nombre_Y_SituacionProf <- data.frame("APELLIDOS Y NOMBRES" = profsConSituacion$APELLIDOSYNOMBRES,
                                  "SITUACIÓN ADMINISTRATIVA 2023 1" = profsConSituacion$NOMBREFORMATO)
  
Nombre_Y_CargoProf <- data.frame("APELLIDOS Y NOMBRES" = cargos$`APELLIDOS Y NOMBRES`,
                                 "CARGO ACADÉMICO ADMNISTRATIVO 2023-1" = cargos$`CARGO FUNCION`)
  
planta_docente_toworkwith <- data.frame(planta_docente_toworkwith)
  
planta_docente_toworkwith$CARGO.ACADÉMICO.ADMNISTRATIVO.2023.1 <- as.character(planta_docente_toworkwith$CARGO.ACADÉMICO.ADMNISTRATIVO.2023.1)
planta_docente_toworkwith$SITUACIÓN.ADMINISTRATIVA.2023.1 <- as.character(planta_docente_toworkwith$SITUACION.ADM...CARGO.ACDM.2023.1)
  
colnames(planta_docente_toworkwith)
  
planta_docente_toworkwith <- rows_update(planta_docente_toworkwith, Nombre_Y_CargoProf, 
                                         by = "APELLIDOS.Y.NOMBRES",
                                           unmatched = "ignore")
planta_docente_toworkwith <- rows_update(planta_docente_toworkwith, Nombre_Y_SituacionProf,
                                         by = "APELLIDOS.Y.NOMBRES",
                                         unmatched =  "ignore")
  
planta_docente_toworkwith <- planta_docente_toworkwith %>%
  mutate(
    `SITUACION.ADM...CARGO.ACDM.2023.1` = if_else(
      !is.na(`CARGO.ACADÉMICO.ADMNISTRATIVO.2023.1`) | !is.na(`SITUACIÓN.ADMINISTRATIVA.2023.1`),
      1,
      `SITUACION.ADM...CARGO.ACDM.2023.1`
    )
  )
  
  
colnames(planta_docente_toworkwith)
  
  
situaCargoData <- data.frame("IDENTIFICACIÓN" = planta_docente_toworkwith$IDENTIFICACION, 
                             "CARGO ACADÉMICO ADMINISTRATIVO 2023-1" =planta_docente_toworkwith$CARGO.ACADÉMICO.ADMNISTRATIVO.2023.1,
                             "SITUACIÓN ADMINISTRATIVA 2023-1" = planta_docente_toworkwith$SITUACIÓN.ADMINISTRATIVA.2023.1,
                             "SITUACION.ADM...CARGO.ACDM.2023.1" = planta_docente_toworkwith$SITUACION.ADM...CARGO.ACDM.2023.1
                             )
  
  
  
### Now we shall deal with hours, students... per Prof. Also we have to solve the problems of
# The names with different written style
  
namesActivities <- unique(rawData$NOMBRE_ASS)
  
  
exceptionCriteria <- c('Trabajo de Grado', 'Proyecto de tesis', 'Tesis', 'tesis', 'Pasantía', 'Examen', 'Especialidad', 'especialización', 'final',
                       'proyecto de grado', 'práctica profesional', 'posgrado')
  
  
first_Filter_criteria <- list()
polished_filter <- list()
  
for(i in namesActivities){
  for(j in exceptionCriteria) {
    if (grepl(j,i, ignore.case = TRUE)){
      first_Filter_criteria[[length(first_Filter_criteria) + 1]] = i
    }
  }
}
  
  
  
  
polished_filter_indices <- grep("seminario", first_Filter_criteria, ignore.case = TRUE)
first_Filter_criteria <- first_Filter_criteria[-polished_filter_indices]
first_Filter_criteria <-unique(first_Filter_criteria)
  
  
particular_elements_indices <- c(5,6,19,20,53,64,65)
  
first_Filter_criteria <- first_Filter_criteria[-particular_elements_indices]
first_Filter_criteria <- c(first_Filter_criteria, "Tesis de Maestría")


SummarizedProfInfo <- data.frame(
  `IDENTIFICACIÓN` = c(),
  `APELLIDOS Y NOMBRES`= c(),
  `CURSOS PREGRADO 2023-1`= c(),
  `HORAS PREGRADO 2023-1` = c(),
  `ESTUDIANTES PREGRADO 2023-1` = c(),
  `CURSOS PEAMA 2023-1` = c()
)
  
for (name in unique(rawData$PPAL_NOMPRS)){
  pregradInfoPerProf <- rawData %>%
    select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`, Pregrado, `NÚMERO DE INSCRITOS ACTUAL`, `NÚMERO DE HORAS SEMANALES`, PPAL_DOC_DOCENTE) %>%
    filter(
      PPAL_NOMPRS == name,
      !(NOMBRE_ASS %in% first_Filter_criteria), 
      !is.na(Pregrado),
      !is.na(`NÚMERO DE INSCRITOS ACTUAL`),
    ) %>%
    mutate(
      `NÚMERO DE HORAS SEMANALES` = if_else(
        is.na(`NÚMERO DE HORAS SEMANALES`),
        2,
        `NÚMERO DE HORAS SEMANALES`
      )
    )
    
  if (nrow(pregradInfoPerProf) != 0){
    numLecturesPregrado <- pregradInfoPerProf %>% 
      dplyr::count(Pregrado)
      
    newRow <- data.frame(
      IDENTIFICACIÓN = pregradInfoPerProf$PPAL_DOC_DOCENTE[[1]],
      `APELLIDOS Y NOMBRES`= pregradInfoPerProf$PPAL_NOMPRS[[1]],
      `CURSOS PREGRADO 2023-1` = numLecturesPregrado$n,
      `HORAS PREGRADO 2023-1`= sum(as.numeric(pregradInfoPerProf$`NÚMERO DE HORAS SEMANALES`)),
      `ESTUDIANTES PREGRADO 2023-1` = sum(as.numeric(pregradInfoPerProf$`NÚMERO DE INSCRITOS ACTUAL`)),
      `CURSOS PEAMA 2023-1` = NA
    )
      
    SummarizedProfInfo <- rbind(SummarizedProfInfo, newRow)
  }
}
  
SummarizedProfInfo

  #Now we do the same but for the graduate info
  
  
SummarizedProfInfoGrad <- data.frame(
  `IDENTIFICACIÓN` = c(),
  `APELLIDOS Y NOMBRES`= c(),
  `CURSOS POSGRADO 2023-1`= c(),
  `HORAS POSGRADO 2023-1` = c(),
  `ESTUDIANTES POSGRADO 2023-1` = c(),
  `CURSOS PEAMA 2023-1` = c()
)
  
  
  

for (name in unique(rawData$PPAL_NOMPRS)){
  gradInfoPerProf <- rawData %>%
    select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`, Pregrado ,`Postgrados y másteres`, `NÚMERO DE INSCRITOS ACTUAL`, `NÚMERO DE HORAS SEMANALES`, PPAL_DOC_DOCENTE) %>%
    filter(
      PPAL_NOMPRS == name,
      !(NOMBRE_ASS %in% first_Filter_criteria), 
      is.na(Pregrado),
      !is.na(`Postgrados y másteres`),
      !is.na(`NÚMERO DE INSCRITOS ACTUAL`)
    ) %>%
    mutate(
      `NÚMERO DE HORAS SEMANALES` = if_else(
        is.na(`NÚMERO DE HORAS SEMANALES`),
        2,
        `NÚMERO DE HORAS SEMANALES`
      )
    )
    
  if (nrow(gradInfoPerProf) != 0){
    
    numLecturesPosgrado <- gradInfoPerProf %>%
      dplyr::count(`Postgrados y másteres`)
      
    newRow <- data.frame(
      IDENTIFICACIÓN = gradInfoPerProf$PPAL_DOC_DOCENTE[[1]],
      `APELLIDOS Y NOMBRES` = gradInfoPerProf$PPAL_NOMPRS[[1]],
      `CURSOS POSGRADO 2023-1`= numLecturesPosgrado$n,
      `HORAS POSGRADO 2023-1` = sum(as.numeric(gradInfoPerProf$`NÚMERO DE HORAS SEMANALES`)),
      `ESTUDIANTES POSGRADO 2023-1` =  sum(as.numeric(gradInfoPerProf$`NÚMERO DE INSCRITOS ACTUAL`)),
      `CURSOS PEAMA 2023-1` = NA
    )
      
    SummarizedProfInfoGrad <- rbind(SummarizedProfInfoGrad, newRow)
      
  }
}
  
  
totalData <- merge(SummarizedProfInfo, SummarizedProfInfoGrad, by = c("APELLIDOS.Y.NOMBRES", "IDENTIFICACIÓN", "CURSOS.PEAMA.2023.1"), all.x = TRUE, all.y = TRUE )
colnames(totalData)
  
write_xlsx(totalData, "test2.xlsx")
  
colnames(rawData)
  
  
  
  
  
## The Peama part
  
consolidado <- read_xlsx("ConsolidadoFinal.xlsx")
  
for (value in unique(rawData$PPAL_NOMPRS)){
  peamaPerProf <- consolidado %>%
    select(`CURSO PEAMA`, DOC_DOCENTE) %>%
    filter( !is.na(`CURSO PEAMA`)) %>%
    group_by(DOC_DOCENTE) %>%
    summarise(PEAMA_count = sum(`CURSO PEAMA` == "PEAMA"))
}
  
write_xlsx(totalData, "DATAPREPEAMA.xlsx")
  
  
peamaPerProf <- data.frame("CURSOS.PEAMA.2023.1" = peamaPerProf$PEAMA_count, "IDENTIFICACIÓN" = peamaPerProf$DOC_DOCENTE )
colnames(peamaPerProf)
peamaPerProf <- peamaPerProf %>%
  filter(IDENTIFICACIÓN %in% planta_docente_toworkwith$IDENTIFICACION)


  
totalData$CURSOS.PEAMA.2023.1 <- as.integer(totalData$CURSOS.PEAMA.2023.1)
addPeama <- rows_update(totalData, peamaPerProf, by = "IDENTIFICACIÓN", unmatched = "ignore")



addSituaCargo <- merge(situaCargoData, addPeama, by = "IDENTIFICACIÓN", all.x = TRUE, all.y =  TRUE)

write_xlsx(addSituaCargo, "addSituaCargo.xlsx")

  
###############################################################################
## Now comes the tricky part, relating the names with the different writting


##############################################################################
#######################################################


ids_planta_docente <- planta_docente_toworkwith$IDENTIFICACION
index_ids_plan_docente <- match(ids_planta_docente, addSituaCargo$IDENTIFICACIÓN)
notInDataProfPlantaDocente <- c()


for (i in 1:length(index_ids_plan_docente)){
  if (is.na(index_ids_plan_docente[i])){
    notInDataProfPlantaDocente <- c(notInDataProfPlantaDocente, i)
  }
}



finalDataFrame <- data.frame()


for (i in index_ids_plan_docente){
  finalDataFrame <- rbind(finalDataFrame, addSituaCargo[i, ])
}



finalDataFrame$IDENTIFICACION <- finalDataFrame$IDENTIFICACIÓN
finalDataFrame$IDENTIFICACIÓN <- NULL

finalDataFrame$CARGO.ACADÉMICO.ADMNISTRATIVO.2023.1 <- finalDataFrame$CARGO.ACADÉMICO.ADMINISTRATIVO.2023.1
finalDataFrame$CARGO.ACADÉMICO.ADMINISTRATIVO.2023.1 <- NULL

finalDataFrame$APELLIDOS.Y.NOMBRES <- NULL

common_columns <- intersect(colnames(planta_docente_toworkwith), colnames(finalDataFrame))
common_columns <- common_columns[-1]
common_columns

na.omit(planta_docente_toworkwith["CURSOS.PEAMA.2023.1"])

for(col in common_columns){
  planta_docente_toworkwith[[col]] <- finalDataFrame[[col]]
}




DEDICACIÓN_ORGANIZED <- c("DOCENTE DEDICAC. EXCLUSIVA", 
                          "DOCENTE TIEMPO COMPLETO",
                          "DOCENTE MEDIO TIEMPO",
                          "DOCENTE CATEDRA 0,7",
                          "DOCENTE CATEDRA 0,6",
                          "DOCENTE CATEDRA 0,5",
                          "DOCENTE CATEDRA 0,4",
                          "DOCENTE CATEDRA 0,3",
                          "DOCENTE CATEDRA 0,2",
                          "DOCENTE CATEDRA 0,1",
                          "DOCENTE CATEDRA 0,0")


HORASDOCENCIA <- c(44,40,20,21,18,15,12,9,6,3,0)

tabla_DED_HOR <- table(DEDICACIÓN_ORGANIZED, HORASDOCENCIA)


cursos_esperados <- c(2,2,2,2,2,2,2,2,1,1,0)
tabla_curs_esp <- table(DEDICACIÓN_ORGANIZED, cursos_esperados)


finalProduct <- planta_docente_toworkwith %>%
  mutate(
    CURSOS.REGISTRADOS.2023.1 = rowSums(select(., CURSOS.PREGRADO.2023.1, CURSOS.POSGRADO.2023.1), na.rm = TRUE),
    HORAS.REGISTRADAS.2023.1 = rowSums(select(., HORAS.PREGRADO.2023.1, HORAS.POSGRADO.2023.1), na.rm = TRUE),
    TOTAL.HORAS.DOCENCIA.2023.1 = HORASDOCENCIA[match(DEDICACIÓN, DEDICACIÓN_ORGANIZED)],
    HORAS.SITUACION.ADM...CARGO.ACDM.2023.1 =ifelse(!is.na(TOTAL.HORAS.DOCENCIA.2023.1) & !is.na(SITUACION.ADM...CARGO.ACDM.2023.1),
                                                    TOTAL.HORAS.DOCENCIA.2023.1 *SITUACION.ADM...CARGO.ACDM.2023.1, NA),
    TOTAL.HORAS.DISPONIBLE.2023.1 = coalesce(TOTAL.HORAS.DOCENCIA.2023.1, 0) - coalesce(HORAS.SITUACION.ADM...CARGO.ACDM.2023.1, 0),
    CURSOS.ESPERADOS.2023.1 = cursos_esperados[match(DEDICACIÓN, DEDICACIÓN_ORGANIZED)],
    DIFERENCIA.CURSOS.2023.1 = coalesce(CURSOS.ESPERADOS.2023.1,0) - coalesce(CURSOS.REGISTRADOS.2023.1,0) - coalesce(CURSOS.PEAMA.2023.1,0)
  )


colnames(finalProduct) <- colnames(copy_of_planta_docente)


length(colnames(finalProduct))

noInfoProfs <- finalProduct %>%
  filter(is.na(`CURSOS PREGRADO 2023-1`),	
         is.na(`HORAS PREGRADO 2023-1`),	
         is.na(`ESTUDIANTES PREGRADO 2023-1`),	
         is.na(`CURSOS POSGRADO 2023-1`),	
         is.na(`HORAS POSGRADO 2023-1`),
         is.na(`ESTUDIANTES POSGRADO 2023-1`))



infoProfsNotFound <- rawData[match(noInfoProfs$IDENTIFICACION, rawData$PPAL_DOC_DOCENTE),]

gradInfoProfsNotFound <- infoProfsNotFound %>%
  filter( !is.na(`Postgrados y másteres`),
          is.na(Pregrado),
          NOMBRE_ASS %in% first_Filter_criteria ) %>%
  select(PPAL_DOC_DOCENTE, NOMBRE_ASS ) 
  


materiasPosgradoProfNotFound <- rawData %>%
  filter(PPAL_DOC_DOCENTE %in% gradInfoProfsNotFound$PPAL_DOC_DOCENTE) %>%
  select(PPAL_DOC_DOCENTE,NOMBRE_ASS) %>%
  group_by(PPAL_DOC_DOCENTE) %>%
  reframe(Asignaturas = toString(NOMBRE_ASS))

 


finalProduct <- finalProduct %>%
  mutate(`OBSERVACION 2023-1` = if_else(IDENTIFICACION %in% gradInfoProfsNotFound$PPAL_DOC_DOCENTE,
                                        materiasPosgradoProfNotFound$Asignaturas[match(IDENTIFICACION,materiasPosgradoProfNotFound$PPAL_DOC_DOCENTE )],
                                       `OBSERVACION 2023-1`)
         )


restOfNotFoundInfoProf <- noInfoProfs %>%
  filter(!(IDENTIFICACION %in% gradInfoProfsNotFound$PPAL_DOC_DOCENTE))


finalProduct <- finalProduct %>%
  mutate(`OBSERVACION 2023-1` = if_else(IDENTIFICACION %in% restOfNotFoundInfoProf$IDENTIFICACION,
                                        "No está en SIA",
                                        `OBSERVACION 2023-1`)
  ) 


write_xlsx(finalProduct, "EntregaFinal2023-2.xlsx")



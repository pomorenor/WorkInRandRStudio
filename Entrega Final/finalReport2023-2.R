setwd("C:/Users/pohlj/OneDrive/Escritorio/Job/WorkInRandRStudio/Entrega Final")

library("dplyr")
library("readxl")
library("writexl")
library("stringdist")
library("openxlsx")
library("gtools")

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
  filter(`FECHA FINAL` == max(`FECHA FINAL`) | `FECHA FINAL` %in% fechaspreviasconsideradas)

profsConSituacion <- profsConSituacion %>%
  group_by(APELLIDOSYNOMBRES) %>%
  filter(`FECHA FINAL` == max(`FECHA FINAL`))

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
                             "SITUACION.ADM...CARGO.ACDM.2023.1" = planta_docente_toworkwith$SITUACION.ADM...CARGO.ACDM.2023.1)



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
        "2",
        `NÚMERO DE HORAS SEMANALES`
      )
    )
  
  if (nrow(pregradInfoPerProf) != 0){
    numLecturesPregrado <- pregradInfoPerProf %>% 
      count(Pregrado)
    
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
write_xlsx(SummarizedProfInfo, "SummarizedProfInfo.xlsx")

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
      is.na(Pregrado),
      !is.na(`Postgrados y másteres`),
      !is.na(`NÚMERO DE INSCRITOS ACTUAL`)
    ) %>%
    mutate(
      `NÚMERO DE HORAS SEMANALES` = if_else(
        is.na(`NÚMERO DE HORAS SEMANALES`),
        "2",
        `NÚMERO DE HORAS SEMANALES`
      )
    )
  
  if (nrow(gradInfoPerProf) != 0){
    
    numLecturesPosgrado <- gradInfoPerProf %>%
      count(`Postgrados y másteres`)
    
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


## The Peama part

consolidado <- read_xlsx("Consolidado.xlsx")

for (value in unique(rawData$PPAL_NOMPRS)){
  peamaPerProf <- consolidado %>%
    select(DOCENTE, `CURSO PEAMA`) %>%
    filter( !is.na(`CURSO PEAMA`)) %>%
    group_by(DOCENTE) %>%
    summarise(PEAMA_count = sum(`CURSO PEAMA` == "PEAMA"))
}


peamaPerProf <- data.frame("APELLIDOS Y NOMBRES" = peamaPerProf$DOCENTE, "CURSOS PEAMA 2023-1" = peamaPerProf$PEAMA_count )

addPeama <- merge(totalData, peamaPerProf, by = c("APELLIDOS.Y.NOMBRES", "CURSOS.PEAMA.2023.1"), all.x = TRUE, all.y = TRUE)

addSituaCargo <- merge(addPeama, situaCargoData, by = "IDENTIFICACIÓN")


write_xlsx(addSituaCargo, "test5.xlsx")


###############################################################################
## Now comes the tricky part, relating the names with the different writting

namesPlanta <- tolower(planta_docente_toworkwith$APELLIDOS.Y.NOMBRES)
namesRaw <- tolower(unique(rawData$PPAL_NOMPRS))



compendium_match_indices <- data.frame(NamePlanta = c(),
                            NameRaw = c(),
                            Distance = c(),
                            Match = c(),
                            RawIndex = c()
                            )



for(name in namesPlanta){
  
  match_indices <- data.frame(NamePlanta = c(),
                              NameRaw = c(),
                              Distance = c(),
                              Match = c(),
                              RawIndex = c()
                             )
  
  for(prenom in namesRaw){
    match <- stringdist::amatch(name, prenom, method = "jaccard")
    dist <- stringdist(name, prenom, method = "jaccard")
    raw_index <- which(namesRaw == prenom)
    if(length(raw_index) == 0){
      raw_index <- NA
    }
    new_row <- data.frame(NamePlanta = name, NameRaw = prenom, Distance = dist, Match = match,
                          RawIndex = raw_index)
    match_indices <- rbind(match_indices, new_row)
    }
  
  best_match_data <- match_indices[which.min(match_indices$Distance), ]
  compendium_match_indices <- rbind(compendium_match_indices, best_match_data )
}


##############################################################################3


compendium_match_indices[["NamePlantaIndex"]] <- 1:length(namesPlanta)


matching_errors <- compendium_match_indices %>%
  select(NamePlanta, NameRaw, Distance, Match, RawIndex, NamePlantaIndex) %>%
  filter(Distance != 0)
  


#write_xlsx(matching_errors, "ErrorsInMatching.xlsx")
#write_xlsx(compendium_match_indices, "NamesPlaneAndRaw.xlsx")
#write_xlsx(totalData, "InformacionCompleta.xlsx")
  
notFoundProfessor <- read_xlsx("ErrorsInMatching.xlsx")


nums <- c(compendium_match_indices$RawIndex)

realnames <- unique(rawData$PPAL_NOMPRS)[compendium_match_indices$RawIndex]
realIndices <- match(realnames,totalData$APELLIDOS.Y.NOMBRES)




finalProduct <- totalData[realIndices, ]
finalProduct$APELLIDOS.Y.NOMBRES <- planta_docente_toworkwith$APELLIDOS.Y.NOMBRES

colnames(finalProduct)
colnames(planta_docente_toworkwith)

planta_docente_toworkwith <- merge(planta_docente_toworkwith, finalProduct, 
                                         by = "APELLIDOS.Y.NOMBRES",
                                         all.x = TRUE, 
                                         all.y = TRUE) 

write_xlsx(planta_docente_toworkwith, "test3.xlsx")



#######################################################3

totalData

ids_planta_docente <- planta_docente_toworkwith$IDENTIFICACION

names_raw_data <- unique(rawData$PPAL_NOMPRS)
ids_names_raw_data <- match(names_raw_data, )

rawData_Ids <- rawData %>%
  select(PPAL_NOMPRS, PPAL_DOC_DOCENTE) %>% 
  filter(PPAL_NOMPRS %in% names_raw_data)

rawData_Ids$PPAL_DOC_DOCENTE

associated_index <- match(ids_planta_docente, rawData_Ids$PPAL_DOC_DOCENTE)

associated_index
newProduct <- totalData[associated_index, ]


write_xlsx(newProduct, "NewProduct.xlsx")

names_raw_data[590]

ids_planta_docente[1]
rawData_Ids
match(35457094, totalData$IDENTIFICACIÓN   )
totalData

library("dplyr")
library("readxl")
library("writexl")

rawData <- read_xlsx("PrincipalData.xlsx")
preparedData <- read_xlsx("2023-2ToFill.xlsx")
consolidado <- read_xlsx("Consolidado.xlsx")


namesRawData <- rawData$PPAL_NOMPRS
namesPreparedData <- preparedData$`APELLIDOS Y NOMBRES`

#Now we convert all of them to lower case letter so we can compare them
#However we don't do it for raw data because we have to filter it first

#lowercase_rawDataNames <- tolower(namesRawData)
lowercase_preparedDataNames <- tolower(namesPreparedData)

#First we are gonna establish the filters, we start by obtaining the
#unique names of the prof. activities 

namesActivities <- unique(rawData$NOMBRE_ASS)

namesTipologies <- unique(consolidado$TIPOLOGÍAS)
namesAsignaturas <- unique(consolidado$ASIGNATURA)


exceptionCriteria <- c('Trabajo de Grado', 'Proyecto de tesis', 'Tesis', 'tesis', 'Pasantía', 'Examen', 'Especialidad', 'especialización', 'final',
                       'proyecto de grado', 'especialidades', 'práctica profesional', 'posgrado')

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

first_Filter_criteria

# For now it is fine. Ich muss einige Sätze korrigieren


# We create the dataframe to store the compendium of info

SummarizedProfInfo <- data.frame(
  `NOMBRES Y APELLIDOS`= c(),
  `HORAS PREGRADO 2023-1` = c()
)

for (value in unique(rawData$PPAL_NOMPRS)){
  numOfLecturesPerProf <- rawData %>% 
    select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`, Pregrado, `NÚMERO DE INSCRITOS ACTUAL`, `NÚMERO DE HORAS SEMANALES`) %>%
    filter(PPAL_NOMPRS == value, 
           !(NOMBRE_ASS %in% first_Filter_criteria), 
           !is.na(Pregrado),
           !is.na(`NÚMERO DE INSCRITOS ACTUAL`),
           ) %>%
    mutate(
      `NÚMERO DE HORAS SEMANALES` =if_else(
        is.na(`NÚMERO DE HORAS SEMANALES`),
        "2",
        `NÚMERO DE HORAS SEMANALES`
      )
    )

  
  if (nrow(numOfLecturesPerProf) != 0){
  
    newRow <- data.frame(
      `NOMBRES Y APELLIDOS`= numOfLecturesPerProf$PPAL_NOMPRS[[1]],
      `HORAS PREGRADO 2023-1`= sum(as.numeric(numOfLecturesPerProf$`NÚMERO DE HORAS SEMANALES`))
    )
    
    SummarizedProfInfo <- rbind(SummarizedProfInfo, newRow)
  }
}                     

## Now we cross our summarized information with what we've got in the form to fill 




### Finally we print to an excel file   


excel_output <- "SumInfo.xlsx"
write_xlsx(SummarizedProfInfo, excel_output)


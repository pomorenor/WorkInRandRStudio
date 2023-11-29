library("dplyr")
library("readxl")

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


exceptionCriteria <- c('Trabajo de Grado', 'Proyecto de tesis', 'Tesis', 'tesis', 'Pasantía', 'Examen')
exceptionCriteria2 <- c('Seminario', 'seminario')

first_Filter_criteria <- list()
polished_filter <- list()

for(i in namesActivities){
  for(j in exceptionCriteria) {
    if (grepl(j,i)){
      first_Filter_criteria[[length(first_Filter_criteria) + 1]] = i
    }
  }
}

for(i in first_Filter_criteria){
  for(j in exceptionCriteria2) {
    if ( j %in% i  == FALSE){
      polished_filter[[length(polished_filter) + 1]] = i
    }
  }
}

polished_filter










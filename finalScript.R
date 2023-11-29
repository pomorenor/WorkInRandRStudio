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

  







############ Let us leave this for later
exceptionCriteria <- c('Tesis', 'tesis', 'Pasantía', 'Proyecto', 'Grado', 'Examen', 'Posgrado')
first_Filter_criteria <- list()


for(i in namesActivities){
  for(j in exceptionCriteria) {
    if (grepl(j,i)){
      first_Filter_criteria[[length(first_Filter_criteria) + 1]] = i
    }
  }
}

first_Filter_criteria

library("dplyr")
library("readxl")

#Some functions we require 

isNotBlank <- function(x) {
  nchar(trimws(x)) > 0
}




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


numOfLecturesPerProf <- rawData %>% 
  select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`, Pregrado, `NÚMERO DE INSCRITOS ACTUAL`, `NÚMERO DE HORAS SEMANALES`) %>%
  filter(PPAL_NOMPRS == "Angelica Maria Gonzalez Clavijo", 
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

numOfLecturesPerProf

totalHoursPreg <- sum(as.numeric(numOfLecturesPerProf$`NÚMERO DE HORAS SEMANALES`))



library("dplyr")
library("ggplot2")
library("readxl")


raw_data <- read_xlsx("PrincipalData.xlsx")
dataToFill <- read_xlsx("C:/Users/pohlj/OneDrive/Escritorio/Job/2023-2ToFill.xlsx")

exceptionCriteria <- c('Trabajo de Grado', 'Proyecto de tesis', 'Tesis', 'tesis', 'Pasantía', 'Examen')
finalCriteria <- list()



numCountsPerProfessor <- table(raw_data$PPAL_NOMPRS) 
typesAndNamesNOMASS <- table(raw_data$NOMBRE_ASS)

typesAndNamesNOMASS

names(raw_data)

numOfLecturesPerProf <- raw_data %>% 
  select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`) %>%
  filter(PPAL_NOMPRS == "Liliana Alejandra Chicaiza Becerra")

print(numOfLecturesPerProf)

typesAndNamesNOMASS




for(i in names(typesAndNamesNOMASS)){
  for(j in exceptionCriteria) {
    if (grepl(j,i)){
      finalCriteria[[length(finalCriteria) + 1]] = i
    }
  }
}

finalCriteriaUniques <- unique(finalCriteria)
finalCriteriaUniques


uniqueNames <- unique(raw_data$PPAL_NOMPRS)
uniqueNames

totalInfo <- list()

for (i in uniqueNames) {
  numOfLecturesPerProf <- raw_data %>% 
  select(PPAL_NOMPRS, NOMBRE_ASS, `NÚMERO DE HORAS SEMANALES`) %>%
  filter(PPAL_NOMPRS == i)
  
  totalInfo[[i]] <- numOfLecturesPerProf
}


finalReport <- bind_rows(totalInfo)
finalReport


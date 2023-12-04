library("dplyr")
library("readxl")
library("writexl")
library("stringdist")

rawData <- read_xlsx("PrincipalData.xlsx")
preparedData <- read_xlsx("2023-2ToFill.xlsx")
consolidado <- read_xlsx("Consolidado.xlsx")


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


# Now we search for PEAMA

for (value in unique(rawData$PPAL_NOMPRS)){
  peamaPerProf <- consolidado %>%
    select(DOCENTE, `CURSO PEAMA`) %>%
    filter( !is.na(`CURSO PEAMA`)) %>%
    group_by(DOCENTE) %>%
    summarise(PEAMA_count = sum(`CURSO PEAMA` == "PEAMA"))
}


peamaPerProf 

SummaryProfNames <- tolower(SummarizedProfInfo$NOMBRES.Y.APELLIDOS)
PeamaProfNames <- tolower(peamaPerProf$DOCENTE)

EqualIndices <- c()

for (name in PeamaProfNames){
  matches <- stringdist::amatch(name, SummaryProfNames, method = "jaccard")
  EqualIndices <- c(EqualIndices, matches)
}

FinalEqualIndices <- EqualIndices[!is.na(EqualIndices)]

columnPeama <- vector("numeric", length = length(SummarizedProfInfo$NOMBRES.Y.APELLIDOS) )

for (i in 1:length(columnPeama)){
    columnPeama[i] <- NA
}

for (value in FinalEqualIndices){
  columnPeama[value] <- peamaPerProf$PEAMA_count[value]
} 

columnPeama

finalDT <- data.frame(SummarizedProfInfo$NOMBRES.Y.APELLIDOS, SummarizedProfInfo$HORAS.PREGRADO.2023.1, PEAMA = columnPeama)

finalDT

## Now we cross our summarized information with what we've got in the form to fill 
## For that we have to compare the names in both lists 

namesRawData <- tolower(SummarizedProfInfo$NOMBRES.Y.APELLIDOS)
namesPreparedData <- tolower(preparedData$`APELLIDOS Y NOMBRES`)

vec_indices_comparison <- c()

for (name in namesPreparedData) {
  matches <- stringdist::amatch(name,  namesRawData, method = "jaccard",   maxDist = 1)
  vec_indices_comparison <- c(vec_indices_comparison, matches)
}

final_vec_indices_comparison <- vec_indices_comparison[!is.na(vec_indices_comparison)]
final_vec_indices_comparison



#preparedData$`APELLIDOS Y NOMBRES`[[1]]
namesRawData[[439]]
#SummarizedProfInfo[539,  ]


totalNombresProf <- namesRawData[final_vec_indices_comparison]
totalHorasPregradoPorProf <- SummarizedProfInfo$HORAS.PREGRADO.2023.1[final_vec_indices_comparison]

finalDF <- data.frame(totalNombresProf, totalHorasPregradoPorProf)

### Finally we print to an excel file   

finalDF[3, ]

excel_output <- "SumInfo.xlsx"
write_xlsx(finalDF, excel_output)
write_xlsx(SummarizedProfInfo, "pato.xlsx")
write_xlsx(finalDT, "dataConPeama.xlsx")


string1 <- tolower("Pablo Enrique Abril Contreras")
string2 <- tolower("ABRIL CONTRERAS PABLO ENRIQUE")
distance <- stringdist::stringdistmatrix(string1, string2, method = "jaccard")
distance

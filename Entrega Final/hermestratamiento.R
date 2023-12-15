setwd("C:/Users/pohlj/OneDrive/Escritorio/Job/WorkInRandRStudio/Entrega Final")


library("dplyr")
library("readxl")
library("writexl")
library("stringdist")
library("gtools")
library("matrixStats")

reporte_de_docentes <- read_xlsx("Hermes.xlsx", sheet = "REPORTE DE DOCENTES")
informes <- read_xlsx("Hermes.xlsx", sheet = "Informes")


nonEmptyPercentageInformes <- informes %>%
  filter()
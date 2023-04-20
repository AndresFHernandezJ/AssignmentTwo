library(tidyverse)
library(caret)
library(gmodels)
#Load Model
knnFit <- readRDS("knn_model.rds")

nuevos_datos <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/XXXXX.csv")
prediccionesX <- predict(knnFit, newdata = nuevos_datos)


CrossTable(nuevos_datos$PARED, prediccionesX, prop.chisq = FALSE)


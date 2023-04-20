Dataset1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_SENSORES_20-68CM.csv")
library(dplyr)

Dataset1_df2 <- Dataset1 %>%
  select(ULTRASONICO, OPTICO.INFRARROJO, DISTANCIA)

# Multilinear Model Training
MultilinearModel <- lm(DISTANCIA ~ ULTRASONICO + OPTICO.INFRARROJO, data = Dataset1_df2)

# Summary
summary(MultilinearModel)

#Coefficients
MultilinearModel$coefficients #-9.13+0.017X+0.0015X

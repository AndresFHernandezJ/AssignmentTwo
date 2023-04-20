library(psych)
library(ggplot2)
# Dataset load
Dataset2 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_PLANA_CONCAVA_CONVEXA_2.csv")
#------------Pre-process your data (if required) and to perform an Exploratory Data Analysis.
# First rows of the dataset
head(Dataset2)

# Dataset Structure
str(Dataset2)

# Summary of the dataset
summary(Dataset2)
#Wall Types
table(Dataset2$PARED)

#Plot library psych
library(psych)
Dataset2_df <- Dataset2
Dataset2_df$DISTANCIA <- as.factor(Dataset2_df$DISTANCIA)

pairs.panels(Dataset2_df,
             main = "Sensor Data (Ultrasónico = Blue, Infrarrojo = Red)",
             pch = 21,
             cex = 2,
             bg = c("blue", "red")[unclass(Dataset2_df$DISTANCIA)],
             bg2 = c("gray80", "pink", "skyblue")[unclass(Dataset2_df$PARED)])


ggplot(Dataset2, aes(x = ULTRASONICO, y = OPTICO.INFRARROJO, color = PARED)) +
  geom_point() +
  labs(title = "Gráfico de dispersión: Sensores y tipos de pared",
       x = "Sensor Ultrasónico", y = "Sensor Óptico Infrarrojo",
       color = "Tipo de pared")

# check proportions of the class levels
prop.table(table(Dataset2$PARED))

hist(Dataset2$ULTRASONICO, breaks = 50)
hist(Dataset2$OPTICO.INFRARROJO, breaks = 50)




library(psych)
# Dataset load
Dataset1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_SENSORES_20-68CM.csv")
#------------Pre-process your data (if required) and to perform an Exploratory Data Analysis.
# First rows of the dataset
head(Dataset1)

# Dataset Structure
str(Dataset1)

# Summary of the dataset
summary(Dataset1)

#Plot library psych
Dataset1_df <- Dataset1
Dataset1_df$DISTANCIA <- as.factor(Dataset1_df$DISTANCIA)

pairs.panels(Dataset1_df[1:3]
             ,main = "Sensor Data (Ultrasónico = Blue, Infrarrojo = Red)"
             ,pch = 21
             ,cex = 2
             ,bg = c("blue", "red")[unclass(Dataset1_df$DISTANCIA)])

#Plots
plot(Dataset1_df$ULTRASONICO, Dataset1_df$OPTICO.INFRARROJO,
     main = "Gráfico de Dispersión: Sensor Ultrasónico vs Sensor Óptico Infrarrojo",
     xlab = "Sensor Ultrasónico", ylab = "Sensor Óptico Infrarrojo")

plot(Dataset1_df$ULTRASONICO, Dataset1_df$DISTANCIA,
     main = "Gráfico de Dispersión: Sensor Ultrasónico vs Distancia",
     xlab = "Sensor Ultrasónico", ylab = "Distacia")

plot(Dataset1_df$OPTICO.INFRARROJO, Dataset1_df$DISTANCIA,
     main = "Gráfico de Dispersión: Sensor Óptico Infrarrojo vs Distancia",
     xlab = "Sensor Óptico Infrarrojo", ylab = "Distacia")

#Histograms
hist(Dataset1$ULTRASONICO,
     main = "Histograma: Ultrasónico",
     xlab = "Ultrasónico ", ylab = "Frecuencia")

hist(Dataset1$OPTICO.INFRARROJO,
     main = "Histograma: Ultrasónico",
     xlab = "Ultrasónico ", ylab = "Frecuencia")

hist(Dataset1$DISTANCIA,
     main = "Histograma: Distancia",
     xlab = "Distancia ", ylab = "Frecuencia")

# Pearson correlation test for ultrasonic sensor
UltrasonicCorr <- cor.test(Dataset1$ULTRASONICO, Dataset1$DISTANCIA, method = "pearson")

# Pearson correlation test for infrared sensor
InfraredCorr <- cor.test(Dataset1$OPTICO.INFRARROJO, Dataset1$DISTANCIA, method = "pearson")


print(UltrasonicCorr)
print(InfraredCorr)

Dataset1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_SENSORES_20-68CM.csv")

# Ultrasonic linear model
UltrasonicM <- lm(DISTANCIA ~ ULTRASONICO, data = Dataset1)

# Infrared linear model
InfraredM <- lm(DISTANCIA ~ OPTICO.INFRARROJO, data = Dataset1)

# Summary
summary(UltrasonicM)
summary(InfraredM)

#Coefficients
UltrasonicM$coefficients # y=-8.84+0.017X
InfraredM$coefficients #-132.73+0.79X

#Plots
plot(Dataset1$ULTRASONICO, Dataset1$DISTANCIA)
abline(UltrasonicM)

plot(Dataset1$OPTICO.INFRARROJO, Dataset1$DISTANCIA)
abline(InfraredM)



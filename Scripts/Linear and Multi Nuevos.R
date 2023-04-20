# Load Models
UltrasonicM <- readRDS("UltrasonicM.rds")
InfraredM <- readRDS("InfraredM.rds")
MultilinearModel <- readRDS("MultilinearModel.rds")

# Load New Data
nuevos_datos1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/XXXX.csv")

# Predict
prediccionesU <- predict(UltrasonicM, nuevos_datos1)
prediccionesI <- predict(InfraredM, nuevos_datos1)
prediccionesM <- predict(MultilinearModel, nuevos_datos1)

# Performance
# RMSE
actual <- nuevos_datos1$DISTANCIA
rmseU <- sqrt(mean((prediccionesU - actual)^2))
rmseI <- sqrt(mean((prediccionesI - actual)^2))
rmseM <- sqrt(mean((prediccionesM - actual)^2))

# Errors
rmseU
rmseI
rmseM

# Prediction tables
# ULTRASONIC
rmseU.df <- data.frame(predicciones = prediccionesU
                  ,actual = nuevos_datos1$DISTANCIA
                  ,SE = (prediccionesU - nuevos_datos1$DISTANCIA))

view(rmseU.df)

# INFRARED
rmseI.df <- data.frame(predicciones = prediccionesI
                       ,actual = nuevos_datos1$DISTANCIA
                       ,SE = (prediccionesI - nuevos_datos1$DISTANCIA))

view(rmseI.df)

# Multilinear
rmseM.df <- data.frame(predicciones = prediccionesM
                       ,actual = nuevos_datos1$DISTANCIA
                       ,SE = (prediccionesM - nuevos_datos1$DISTANCIA))

view(rmseM.df)








Dataset1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_SENSORES_20-68CM.csv")

predictors <- colnames(Dataset1)[-3]

sample.index <- sample(1:nrow(Dataset1)
                       ,nrow(Dataset1)*0.7
                       ,replace = F)


train.data <- Dataset1[sample.index,,drop=F]
test.data <- Dataset1[-sample.index,,drop=F]

########## MODELS #####################

####### ULTRASONIC MODEL #######################
UltrasonicM <- lm(DISTANCIA ~ ULTRASONICO, data = train.data)
UltrasonicM
summary(UltrasonicM)
#Predictions
#ULTRASONIC
predictionsU <- predict(UltrasonicM, test.data)
predictionsU
#Error
RMSEU.df <- data.frame(predicted = predictionsU,
                       actual = test.data$DISTANCIA
                       , SE = (predictionsU - test.data$DISTANCIA))
#promedio de error
sum(RMSEU.df$SE)/nrow(RMSEU.df)
view(RMSEU.df)

####### INFRARED MODEL #####################
InfraredM <- lm(DISTANCIA ~ OPTICO.INFRARROJO, data = train.data)
InfraredM
summary(InfraredM)
#Predictions
#ULTRASONIC
predictionsI <- predict(InfraredM, test.data)
predictionsI
#Error
RMSEI.df <- data.frame(predicted = predictionsI,
                       actual = test.data$DISTANCIA
                       , SE = (predictionsI - test.data$DISTANCIA))
#promedio de error
sum(RMSEI.df$SE)/nrow(RMSEI.df)
view(RMSEI.df)

####### MULTILINEAR MODEL #####################
MultilinearModel <- lm(DISTANCIA ~ ., data = train.data)
MultilinearModel
summary(MultilinearModel)
#Predictions
#ULTRASONIC
predictionsM <- predict(MultilinearModel, test.data)
predictionsM
#Error
RMSEM.df <- data.frame(predicted = predictionsM,
                       actual = test.data$DISTANCIA
                       , SE = (predictionsM - test.data$DISTANCIA))
#promedio de error
sum(RMSEM.df$SE)/nrow(RMSEM.df)
view(RMSEM.df)



sum(RMSEU.df$SE)/nrow(RMSEU.df)
sum(RMSEI.df$SE)/nrow(RMSEI.df)
sum(RMSEM.df$SE)/nrow(RMSEM.df)

# Guardar modelos
saveRDS(UltrasonicM, "UltrasonicM.rds")
saveRDS(InfraredM, "InfraredM.rds")
saveRDS(MultilinearModel, "MultilinearModel.rds")




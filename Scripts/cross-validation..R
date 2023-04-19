Dataset1 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_SENSORES_20-68CM.csv")

predictors <- colnames(Dataset1)[-3]

sample.index <- sample(1:nrow(Dataset1)
                       ,nrow(Dataset1)*0.7
                       ,replace = F)


train.data <- Dataset1[sample.index,,drop=F]
test.data <- Dataset1[-sample.index,,drop=F]

UltrasonicM2 <- lm(DISTANCIA ~ ULTRASONICO, data = train.data)
InfraredM2 <- lm(DISTANCIA ~ OPTICO.INFRARROJO, data = train.data)
MultilinearModel2 <- lm(DISTANCIA ~ ., data = train.data)
MultilinearModel2

summary(MultilinearModel2)

#Predictions
predictions <- predict(MultilinearModel2, test.data)
predictions

#Error
RMSE.df <- data.frame(predicted = predictions,
                      actual = test.data$DISTANCIA
                      , SE = (predictions - test.data$DISTANCIA))

#promedio de error
sum(RMSE.df$SE)/nrow(RMSE.df)

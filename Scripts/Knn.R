library(tidyverse)
library(caret)
library(gmodels)

Dataset2_df <- Dataset2
Dataset2_df$DISTANCIA <- as.factor(Dataset2_df$DISTANCIA)

Dataset2 <- read.csv("D:/Usr/Desktop/Andres/8vo Semestre/DataScience/AssignmentTwo/Datasets/ADQUISION_DATOS_PLANA_CONCAVA_CONVEXA_2.csv")

Dataset2$PARED <- as.factor(Dataset2$PARED)
#index for random sampling
sample.indexW <- sample(1:nrow(Dataset2)
                       ,nrow(Dataset2)*0.7
                       ,replace = F)

predictorsW <- c("ULTRASONICO", "OPTICO.INFRARROJO", "DISTANCIA")

train.dataW <- Dataset2[sample.indexW
                        ,c(predictorsW,"PARED")
                        ,drop=F]

test.dataW <- Dataset2[-sample.indexW
                        ,c(predictorsW,"PARED")
                        ,drop=F]

##### Knn + Normalisation (min-max) #####
ctrl <- trainControl(method="cv", p=0.7)
knnFit <- train(PARED ~ ULTRASONICO+OPTICO.INFRARROJO+DISTANCIA
                , data = train.dataW
                , method = "knn"
                , trControl = ctrl
                , preProcess = c("range")
                , tuneLength = 20)

#Output of kNN fit DESDE ACA
knnFit
#plot(knnFit)

#Get predictions for the testing data
knnPredict <- predict(knnFit, newdata = test.dataW)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, test.dataW$PARED)

CrossTable(test.dataW$PARED, knnPredict, prop.chisq = FALSE)






saveRDS(knnFit, "knn_model.rds")
saveRDS(test.dataW, "test_data")
saveRDS(knnPredict, "knn_predict")


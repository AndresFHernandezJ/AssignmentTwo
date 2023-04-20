normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

library(tidyverse)
# to get current script folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

# Accessing by file address
Pared_PCC <-read_csv(paste0(parentFolder,"/datasets/ADQUISION_DATOS_PLANA_CONCAVA_CONVEXA.csv"))
view(Pared_PCC)

################ One-hot encoding ########
# by logical comparison
colnames(Pared_PCC)[4] <- c("PARED")
Pared_PCC$CONVEX <-
  as.numeric(Pared_PCC$PARED=="CONVEXA;;;;")

Pared_PCC$AXIS <-
  as.numeric(Pared_PCC$PARED=="PLANA;;;;")

Pared_PCC$CONCAVE <-
  as.numeric(Pared_PCC$PARED=="CONCAVA;;;;")

view(Pared_PCC)

# Changing the class variable to factor
Pared_PCC$CONVEX <- as.factor(Pared_PCC$CONVEX)
levels(Pared_PCC$CONVEX)

Pared_PCC$AXIS<- as.factor(Pared_PCC$AXIS)
levels(Pared_PCC$AXIS)

Pared_PCC$CONCAVE <- as.factor(Pared_PCC$CONCAVE)
levels(Pared_PCC$CONCAVE)

#EDA (only 2 numeric features in this data)

#[1]ULTRASONIDO-[2]OPTICO/INFRARROJO
plot(Pared_PCC[1:2])
plot(Pared_PCC[1:2], pch=21
     , bg= c("red", "blue")[unclass(Pared_PCC$CONCAVE)])
plot(Pared_PCC[1:2], pch=21
     , bg= c("red", "yellow")[unclass(Pared_PCC$AXIS)])
plot(Pared_PCC[1:2], pch=21
     , bg= c("red", "green")[unclass(Pared_PCC$CONVEX)])

# check proportions of the class levels
prop.table(table(Pared_PCC$CONVEX))
prop.table(table(Pared_PCC$AXIS))
prop.table(table(Pared_PCC$CONCAVE))

hist(Pared_PCC$ULTRASONICO, breaks = 50)
hist(Pared_PCC$`OPTICO-INFRARROJO`, breaks = 50)

##### Normalisation #####

normData <- Pared_PCC
standardData <- Pared_PCC

### min-max

normData$`OPTICO-INFRARROJO`<-normalise(Pared_PCC$`OPTICO-INFRARROJO`)
normData$ULTRASONICO<-normalise(Pared_PCC$ULTRASONICO)


view(normData)

### z-score standardisation

standardData$`OPTICO-INFRARROJO`<-scale(standardData$`OPTICO-INFRARROJO`)
standardData$ULTRASONICO<- scale(standardData$ULTRASONICO)


#index for random sampling

sample.index <- sample(1:nrow(Pared_PCC) ,nrow(Pared_PCC)*0.7,replace = F)

#training
k <- 3
predictors <- c("ULTRASONICO","OPTICO-INFRARROJO")

# original data
train.data <-
  Pared_PCC[sample.index,c(predictors,"CONVEX","AXIS","CONCAVE"),drop=F]
test.data <- Pared_PCC[-sample.index,c(predictors,"CONVEX","AXIS","CONCAVE"),drop=F]

library(class)
predictionconvex <- knn(train = train.data[predictors]
                  , test = test.data[predictors]
                  ,cl = train.data$CONVEX, k=k)

predictionaxil <- knn(train  = train.data[predictors]
                         , test = test.data[predictors]
                         ,cl = train.data$AXIS, k=k)


predictionconcave<- knn(train = train.data[predictors]
                        , test = test.data[predictors]
                        ,cl = train.data$CONCAVE, k=k)

library(gmodels)
CrossTable(x = test.data$CONCAVE, y = predictionconcave, prop.chisq = F)
CrossTable(x = test.data$AXIS, y = predictionaxil, prop.chisq = F)
CrossTable(x = test.data$CONVEX, y = predictionconvex, prop.chisq = F)

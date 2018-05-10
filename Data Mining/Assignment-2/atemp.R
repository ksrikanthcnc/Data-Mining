library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
fold <- trainControl(method="repeatedcv", number = 3,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(verboseIter = TRUE,
                      savePredictions = TRUE,
                      number = 5
)

adult_data <- read_csv("data.csv")[1:100,]
ent_ratio <- train(income~., data = adult_data, method = "rpart",
                   trControl = fold,
                   layer1=0,
                   layer2=0,
                   layer3=0,
                   hidden = 0)
noprp(ent_ratio$finalModel, box.palette = "Reds")
print("Entropy with 80:20")
matr(ent_ratio)
ent_ratio
mlpML
mlpWeightDecayML
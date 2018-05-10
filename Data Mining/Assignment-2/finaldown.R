#--------------------------------------------------------------------
library(doMC)
library(doParallel)
registerDoMC(cores=3)

while (dev.cur() > 1) dev.off()
rm(list = ls())
matr <- function(model){
  predicted <- predict(model, test, type = 'prob')
  pred=ifelse((predicted[,1]<predicted[,2]), "Low","High")
  #print(confusionMatrix(pred, test$income ))
  conf <- table(pred,test$income)
  cat("Confusion Matrix","\n")
  print(conf)
  r=recall(as.factor(pred), test$income)
  p=precision(as.factor(pred), test$income)
  cat("Accuracy",sum(diag(conf))/sum(conf),"\n")
  cat("Recall",recall(as.factor(pred), test$income),"\n")
  cat("Precision",precision(as.factor(pred), test$income),"\n")
  cat("F-Score",(2*r*p)/(r+p),"\n")
  pred <- predict(model, test, type = 'prob')
  auc<-auc(test$income,predicted[,2])
  plot(roc(test$income,predicted[,2]))
  cat("AUC",auc,"\n")
}
library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
#detach("package:pROC", unload=TRUE)

set.seed(1234)
#---------------------------------------------------------------------
data <- read_csv("data.csv")
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
#View(data)
#summary(data)
#head(data)
#str(data)
#--------------------------------------------------------------------
#80:20 with sampling
intrain <- createDataPartition(y = data$income, p = 0.8, list = FALSE)
train <- data[intrain,]
test <- data[-intrain,]

#10-fold
fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,verboseIter = TRUE,savePredictions = TRUE)
ratio <- trainControl(verboseIter = TRUE, savePredictions = TRUE,number = 5)
#-----------------------------------------------------------------
#models
#info,gini
#tree_ent <- rpart(income~., train_ratio, method = "class", minsplit = 2, minbucket = 1,
#                  parms = list(split = "gini"))
#rpart.plot(tree_ent)

#entropy  
print("MODELS")
print("Entropy")
ent_ratio <- train(income~., data = train, method = "rpart",
                   parms = list(split = "information"),
                   trControl = ratio,
                   tuneLength = 10)
prp(ent_ratio$finalModel, box.palette = "Reds")
print("Entropy with 80:20")
matr(ent_ratio)
ent_fold <- train(income~., data = data, method = "rpart",
                  parms = list(split = "information"),
                  trControl = fold,
                  tuneLength = 10)
prp(ent_fold$finalModel, box.palette = "Reds")
print("Entropy with 10-fold")
matr(ent_fold)

#gini
gini_ratio <- train(income~., data = train, method = "rpart",
                    parms = list(split = "gini"),
                    trControl = ratio,
                    tuneLength = 10)
prp(gini_ratio$finalModel, box.palette = "Reds")
print("gini with 80:20")
matr(gini_ratio)
gini_fold <- train(income~., data = data, method = "rpart",
                   parms = list(split = "gini"),
                   trControl = fold,
                   tuneLength = 10)
prp(gini_fold$finalModel, box.palette = "Reds")
print("gini with fold")
matr(gini_fold)

#Naive Bayes
nb_ratio <- train(income~., data = train,
                  method = "naive_bayes",
                  trControl = ratio,
                  tuneLength = 10)
#not_correct...prp(nb_ratio$finalModel, box.palette = "Reds")
#many plots...plot(nb_ratio$finalModel)
print("NB with ratio")
matr(nb_ratio$finalModel)

nb_fold <- train(income~., data = data,
                 method = "naive_bayes",
                 trControl = fold,
                 tuneLength = 10)
#not_correct...prp(nb_fold$finalModel, box.palette = "Reds")
#many plots...plot(nb_fold$finalModel)
print("NB with fold")
matr(nb_fold$finalModel)

#ANN
prepro(data)

#-------------------------------------------------------------------
data$income <- NULL


#--------------------------------------------------------------------
#library(doMC)
#library(doParallel)
#registerDoMC(cores=3)

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
  lines(roc(test$income,predicted[,2]))
  cat("AUC",auc,"\n")
}
library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(neuralnet)
#detach("package:dplyr", unload=TRUE)

set.seed(1234)
#---------------------------------------------------------------------
data <- read_csv("final.csv")
#data <- data[1:1000,]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)
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
fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(number = 5,
                      verboseIter = TRUE,
                      savePredictions = TRUE,
)
#-----------------------------------------------------------------
#models
#info,gini
#tree_ent <- rpart(income~., train_ratio, method = "class", minsplit = 2, minbucket = 1,
#                  parms = list(split = "gini"))
#rpart.plot(tree_ent)

#entropy  
cat("--------------------MODELS------------------\n")
cat("~~~~~~~~~~Entropy~~~~~~~~\n")
ent_ratio <- train(income~., data = train, method = "rpart",
                   parms = list(split = "information"),
                   trControl = ratio,
                   tuneLength = 10)
prp(ent_ratio$finalModel, box.palette = "Reds")
cat("\nEntropy with 80:20\n")
matr(ent_ratio)
ent_fold <- train(income~., data = data, method = "rpart",
                  parms = list(split = "information"),
                  trControl = fold,
                  tuneLength = 10)
prp(ent_fold$finalModel, box.palette = "Reds")
cat("\nEntropy with 10-fold")
matr(ent_fold)

#gini
cat("~~~~~~~~~~GINI~~~~~~~~~\n")
gini_ratio <- train(income~., data = train, method = "rpart",
                    parms = list(split = "gini"),
                    trControl = ratio,
                    tuneLength = 10)
prp(gini_ratio$finalModel, box.palette = "Reds")
cat("\ngini with 80:20\n")
matr(gini_ratio)
gini_fold <- train(income~., data = data, method = "rpart",
                   parms = list(split = "gini"),
                   trControl = fold,
                   tuneLength = 10)
prp(gini_fold$finalModel, box.palette = "Reds")
cat("\ngini with fold")
matr(gini_fold)

#Naive Bayes
cat("~~~Naive Bayes~~~~~\n")
nb_ratio <- train(income~., data = train,
                  method = "naive_bayes",
                  trControl = ratio,
                  tuneLength = 10)
#not_correct...prp(nb_ratio$finalModel, box.palette = "Reds")
#many plots...plot(nb_ratio$finalModel)
cat("\nNB with ratio\n")
matr(nb_ratio$finalModel)

nb_fold <- train(income~., data = data,
                 method = "naive_bayes",
                 trControl = fold,
                 tuneLength = 10)
#not_correct...prp(nb_fold$finalModel, box.palette = "Reds")
#many plots...plot(nb_fold$finalModel)
cat("\nNB with fold\n")
matr(nb_fold$finalModel)

#ANN
#data <- read_csv("data.csv")
#data <- data[1:1000,]

income <- ifelse( data$income == "Low", 0,1)
data <- data[,-14]
data <- data.frame(data, income)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)

f <- as.formula(paste(" ~", paste(names(data), collapse = " + ")))
m <- model.matrix( f,data = data)
d <- as.data.frame(m)
#d[sapply(d, is.numeric)] <- lapply(d[sapply(d, is.numeric)], scale)

intrain <- createDataPartition(y = d$income, p = 0.8, list = FALSE)
train <- d[intrain,]
test <- d[-intrain,]

n <- names(d)
f <- as.formula(paste("income~", paste(sprintf("`%s`", n[!n %in% "income"]), collapse="+")))
ann <- neuralnet( f,data=train, hidden=5,
                  #                  lifesign = 'full',
                  threshold=0.01)
#plot(ann)
predicted_ <- compute(ann, test[,1:70], rep = 1)
summary(predicted_$net.result)
predicted <- ifelse( predicted_$net.result < 0, 0,1)
cat("Confusion Matrix","\n")
table(predicted,test$income)
conf <- table(predicted,test$income)
print(conf)
acc <- sum(diag(conf))/sum(conf)
cat("Accuracy",acc,"\n")
rec <- conf[1,1]/sum(conf[1,])
cat("Recall",rec,"\n")
pre <- conf[1,1]/sum(conf[,1])
cat("Precision",pre,"\n")
f_s <- 2*conf[1,1]/(2*conf[1,1] + conf[1,2] + conf[2,1])
cat("F_Score",f_s,"\n")


#pred <- prediction(ann, test$income)
#inc <- scale(d$income)
#auc<-auc(test$income,predicted)
#plot(roc(test$income,predicted))
#cat("AUC",auc,"\n")

#folds <- createFolds(d$income)
#str(folds)
#split_up <- lapply(folds, function(ind, d) d[ind,], dat = d)
#split_up


#-------------------------------------------------------------------
library(cluster)
library(flexclust)

d$income <- NULL
#str(d)
print('totss')
print('tot.withinss')
k1 <- kmeans(x = d,
             trace = T,
             centers =  1
)
#k1
k1$totss
k1$tot.withinss

k2 <- kmeans(x = d,
             trace = T,
             centers =  2
)
#k2
k2$totss
k2$tot.withinss

k3 <- kmeans(x = d,
             trace = T,
             centers = 3
)
#k3
k3$totss
k3$tot.withinss

print('clusinfo')
print('silinfo')
p <- pam(x = d,k = 1)
p$clusinfo
p$silinfo
p <- pam(x = d,k = 2)
p$clusinfo
p$silinfo
p <- pam(x = d,k = 3,
         trace.lev = 2)
p$clusinfo
p$silinfo
?pam
p$objective
library(kerasR)
install.packages("kerasR")
?dbscan

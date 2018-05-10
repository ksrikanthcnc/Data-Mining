
install.packages("rpart")
install.packages("rpart.plot");
install.packages("caret")
install.packages("e1071") 
set.seed(1234)
>ind <- sample(2, nrow(finaldata), replace=TRUE, prob=c(0.8, 0.2))
> train <- finaldata[ind==1,]
> test <- finaldata[ind==2,]
> finaldata_rpart <- rpart(PAID_FINE~., method="class", data = train,parms = list(split = "gini"),control = rpart.control(minsplit = 10))
plot(finaldata_rpart)
rpart.plot(finaldata_rpart)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
rpart_predict <- predict(finaldata,test,type="class")
finaldata_rpart <- rpart(PAID_FINE~., method="class", data = train,parms = list(split = "information"),control = rpart.control(minsplit = 10))
install.packages("caret")
trctrl <- trainControl(method = "cv", number = 10)
set.seed(3333)
dtree_fit <- train(PAID_FINE ~., data = train, method = "rpart",
                   parms = list(split = "gini"),
                   trControl=trctrl,
                   tuneLength = 10)
plot(dtree_fit);
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Blues", tweak = 1.2)
test_pred <- predict(dtree_fit, newdata = test
                     confusionMatrix(data = test_pred, test$PAID_FINE))
PredictionsWithClass<- predict(dtree_fit,test,type='class')

>t<-table(predictions=PredictionsWithClass, actual=test$PAID_FINE)
> sum(diag(t))/sum(t) accuaracy
pred<-predict(dtree_fit,test,type='prob')
> auc<-auc(test$PAID_FINE,pred[,2])
> plot(roc(test$PAID_FINE,pred[,2]))
pred
PredictionsWithClass
result <- confusionMatrix(data = test_pred, test$PAID_FINE)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
e1071model <-naiveBayes(PAID_FINE ~., data = train)
> e1071model
e1071modelprediction <- predict(e1071model,test)
> mmetric(test$PAID_FINE,e1071modelprediction,c("ACC","PRECISION","TPR","F1"))
library(rminer)
t<-table(predictions=e1071modelprediction, actual=test$PAID_FINE)
> sum(diag(t))/sum(t) 
ind=createDataPartition(finaldata$PAID_FINE,p=2/3,list=FALSE)
> train<-finaldata[ind,]
> test<-finaldata[-ind,]
> trctrl <- trainControl(method = "cv", number = 10)
> 
  > e1071model <-naiveBayes(PAID_FINE ~., data = train)
> e1071model 
e1071modelprediction <- predict(e1071model,test)
> mmetric(test$PAID_FINE,e1071modelprediction,c("ACC","PRECISION","TPR","F1"))
str(finaldata)
summary(finaldata)
ind <- sample(2, nrow(finaldata), replace=TRUE, prob=c(0.8, 0.2))
> train <- finaldata[ind==1,]
> test <- finaldata[ind==2,]
> table(finaldata$PAID_FINE)
library(rminer)
library(neuralnet)
data <- finaldata
ind <- sample(2, nrow(finaldata), replace = TRUE, prob=c(0.7, 0.3))
trainset = finaldata[ind == 1,]
testset = finaldata[ind == 2,]
trainset$yes= trainset$PAID_FINE == "yes"
trainset$no= trainset$PAID_FINE == "no"
network = neuralnet(yes + no ~ X_COORDINATE + Y_COORDINATE + LATITUDE + LONGITUDE, trainset, hidden=2)
network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)
model<-train(PAID_FINE~. , data=data , trControl=trctrl,method="nb")
model
e1071modelprediction <- predict(e1071model,test,type="raw")
auc<-auc(test$PAID_FINE,e1071modelprediction[,2])
plot(roc(test$PAID_FINE,e1071modelprediction[,2]))
model<-train(PAID_FINE~ + Y_COORDINATE , data=finaldata , trControl=trctrl,method="nb")
pred = predict(model, test, prob=TRUE)
plot(roc(test$PAID_FINE,model[,2]))
finaldata.features = finaldata
> finaldata.features$PAID_FINE <- NULL
> View(finaldata.features)
> results <- kmeans(finaldata.features,2)
> results
finaldata.features = last
> 
  > finaldata.features$PAID_FINE<- NULL
> 
  > View(finaldata.features)
> results <- kmeans(finaldata.features,2)
> D<- daisy(finaldata.features)

> plot(silhouette(results$cluster, D), col=1:2, border=NA)
commands.txt
Displaying commands.txt.
#working
rm(list = ls())
library(readr)
library(e1071)
library(caret)

adult_data <- read_csv("adult-data.csv")
set.seed(1234)

#for this(naive)
adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.factor)

intrain <- createDataPartition(y = adult_data$income, p = 0.8, list = FALSE)
training <- adult_data[intrain,]
testing <- adult_data[-intrain,]

naive <- naiveBayes(income ~ ., data = training)
#class(naive)
#summary(naive)
#print(naive)
predicted <- predict(naive, newdata = testing, type = 'class')
conf <- table(predicted,testing$income)
conf
acc <- sum(diag(conf))/sum(conf)
acc
rec <- conf[1,1]/sum(conf[1,])
rec
pre <- conf[1,1]/sum(conf[,1])
pre
f_s <- 2*conf[1,1]/(2*conf[1,1] + conf[1,2] + conf[2,1])
f_s

library("ROCR")
pred <- prediction(predicted, testing$income)
auc <- performance(pred,"auc")

plot.roc(testing,predicted)

roc(naive,unlist(predicted))
unlist(predicted)

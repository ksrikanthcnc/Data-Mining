#working
rm(list = ls())

library(readr)
adult_data <- read_csv("adult-data.csv")
set.seed(1234)

library(e1071)

adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.factor)

library(caret)
intrain <- createDataPartition(y = adult_data$income, p = 0.7, list = FALSE)
training <- adult_data[intrain,]
testing <- adult_data[-intrain,]

naive <- naiveBayes(income ~ ., data = training)
#class(naive)
#summary(naive)
#print(naive)
pred <- predict(naive, newdata = testing, type = 'class')
conf <- table(pred,testing$income)
conf
acc <- sum(diag(conf))/sum(conf)
acc

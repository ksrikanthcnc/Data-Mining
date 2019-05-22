#workinh entropy, gini(slow)
rm(list = ls())

library(rpart.plot)
library(readr)
library(caret)
#setwd('/home/srikanth/My files/Data Mining/R/Assignment-2')
adult_data <- read_csv("adult-data.csv")
set.seed(1234)

inc = ifelse( adult_data$income == "<=50000", "Low", "High")
adult_data = data.frame(adult_data, inc)
adult_data = adult_data[,-15]

intrain <- createDataPartition(y = adult_data$inc, p = 0.8, list = FALSE)
training <- adult_data[intrain,]
testing <- adult_data[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
dtree_fit_ent <- train(inc~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl = trctrl,
                   tuneLength = 10,verbose=TRUE)
dtree_fit_ent
prp(dtree_fit_ent$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred_ent <- predict(dtree_fit_ent, newdata = testing)
confusionMatrix(test_pred_ent, testing$inc )

dtree_fit_gini <- train(inc~., data = training, method = "rpart",
                          parms = list(split = "gini"),
                          trControl = trctrl,
                          tuneLength = 10)
prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$inc )

predicted <- predict(dtree_fit_gini, testing, type = 'prob')
conf <- table(predicted,testing$inc)
conf
acc <- sum(diag(conf))/sum(conf)
acc
rec <- conf[1,1]/sum(conf[1,])
rec
pre <- conf[1,1]/sum(conf[,1])
pre
f_s <- 2*conf[1,1]/(2*conf[1,1] + conf[1,2] + conf[2,1])
f_s

predicted <- predict(dtree_fit_ent, testing, type = 'raw')
conf <- table(predicted,testing$inc)
conf
acc <- sum(diag(conf))/sum(conf)
acc
rec <- conf[1,1]/sum(conf[1,])
rec
pre <- conf[1,1]/sum(conf[,1])
pre
f_s <- 2*conf[1,1]/(2*conf[1,1] + conf[1,2] + conf[2,1])
f_s


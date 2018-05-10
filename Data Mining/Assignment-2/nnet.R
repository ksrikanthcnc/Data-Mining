rm(list = ls())

library(readr)
adult_data <- read_csv("adult-data.csv")
set.seed(1234)
#str(adult_data)
#native_country <- factor(`native-country`, levels = c( "United-States","Cambodia","England","Puerto-Rico","Canada","Germany","Outlying-US(Guam-USVI-etc)","India","Japan","Greece","South","China","Cuba","Iran","Honduras","Philippines","Italy","Poland","Jamaica","Vietnam","Mexico","Portugal","Ireland","France","Dominican-Republic","Laos","Ecuador","Taiwan","Haiti","Columbia","Hungary","Guatemala","Nicaragua","Scotland","Thailand","Yugoslavia","El-Salvador","Trinadad&Tobago","Peru","Hong","Holand-Netherlands")) 

#adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.factor)
library(caret)
intrain <- createDataPartition(y = adult_data$income, p = 0.8, list = FALSE)
training <- adult_data[intrain,]
testing <- adult_data[-intrain,]

library(nnet)
crs_nnet <- nnet(as.factor(income) ~ ., data = training, size=10, skip=TRUE, trace=FALSE, maxit=1000, MaxNWts=2000)
#summary(crs_nnet)
#plot(crs_nnet)
pred <- predict(crs_nnet, testing, type="class")
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
dtree_fit_ent <- train(income~., data = training, method = "mxnet",
                       trControl = trctrl,
                       tuneLength = 10, maxit=10)
prp(dtree_fit_ent$finalModel, box.palette = "Reds", tweak = 1.2)

conf <- table(pred,testing$income)
conf
acc <- sum(diag(conf))/sum(conf)
acc

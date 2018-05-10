library(doMC)
registerDoMC(cores=3)

rm(list = ls())
#---------------------------------------------------------------------
library(readr)
adult_data <- read_csv("adult-data.csv")
set.seed(1234)

#inc = ifelse( adult_data$income == "<=50000", "Low", "High")
#names(adult_data)
#adult_data = data.frame(adult_data, inc)
#adult_data = adult_data[,-15]
#-----------------------------------------------------------------
#80:20 with sampling
n <- nrow(adult_data)
ads <- adult_data[sample(n),]
train_indices <- 1:round(0.8 * n)
train <- ads[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
test <- ads[test_indices, ]

#80:20 with ...
library(caret)
#caret,train,test
intrain <- createDataPartition(y = adult_data$income, p = 0.8, list = FALSE)
training <- adult_data[intrain,]
testing <- adult_data[-intrain,]

#10-fold
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#------------------------------------------------------------------
#info
predicted <- predict(MODEL-variable, testing, type = 'class')
#confusion matrix,accuracy,recall,precision,f-score
#confusionMatrix(predicted, testing$income )
conf <- table(pred,test$income)
conf
acc <- sum(diag(conf))/sum(conf)
acc
rec <- conf[1,1]/sum(conf[1,])
rec
pre <- conf[1,1]/sum(conf[,1])
pre
f_s <- 2*conf[1,1]/(2*conf[1,1] + conf[1,2] + conf[2,1])
f_s
conf_mat <- confusionMatrix(pred, test$income)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
#---------------------------------------------------------------------
#for this(naive),factorise
adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.factor)
#other
n <- names(dt)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))

sapply(adult_data, is.character)
lapply(adult_data[sapply(adult_data, is.character)], as.factor)
#---------------------------------------------------------------------
#models
#info,gini
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
dtree_fit_ent <- train(inc~., data = training, method = "rpart",
                       parms = list(split = "information"),
                       trControl = trctrl,
                       tuneLength = 10)
prp(dtree_fit_ent$finalModel, box.palette = "Reds", tweak = 1.2)



#---------------------------------------------------------------------
#prune
pruned <- prune(MODEL-variable, cp = 0.05)
rpart.plot(pruned)

library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
data <- read_csv("finalfact.csv")
#data <- data[1:1000,]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
#data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)
#View(data)
#summary(data)
#head(data)
#str(data)
#--------------------------------------------------------------------
#10-fold
fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(
                      verboseIter = TRUE,
                      savePredictions = TRUE,
)

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
f <- as.formula(paste(" ~", paste(names(data), collapse = " + ")))
m <- model.matrix( f,data = data)
data <- as.data.frame(m)

#80:20 with sampling
intrain <- createDataPartition(y = data$income, p = 0.8, list = FALSE)
train <- data[intrain,]
test <- data[-intrain,]

#data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)

#-----------------------------------------------------------------
#models
#info,gini
#tree_ent <- rpart(income~., train_ratio, method = "class", minsplit = 2, minbucket = 1,
#                  parms = list(split = "gini"))
#rpart.plot(tree_ent)

#entropy  
cat("--------------------MODELS------------------\n")
cat("~~~~~~~~~~GINI~~~~~~~~~\n")
gini_ratio <- train(incomeLow~., data = train, method = "rpart",
                    parms = list(split = "gini"),
                    trControl = ratio,
                    tuneLength = 10)
prp(gini_ratio$finalModel, box.palette = "Reds")
cat("\ngini with 80:20\n")
matr(gini_ratio)


model <- gini_ratio

predicted <- predict(model, test)
#pred <- predicted
pred=ifelse((predicted < 0.5), 0,1)
#print(confusionMatrix(pred, test$income ))
conf <- table(pred,test$income)
cat("Confusion Matrix","\n")
print(conf)
acc <- sum(diag(conf))/sum(conf)
r <- conf[1,1]/sum(conf[1,])
p <- conf[1,1]/sum(conf[,1])
cat("Accuracy",sum(diag(conf))/sum(conf),"\n")
cat("Recall",conf[1,1]/sum(conf[1,]),"\n")
cat("Precision",conf[1,1]/sum(conf[,1]),"\n")
cat("F-Score",(2*r*p)/(r+p),"\n")
pred <- predict(model, test)
auc<-auc(test$income,pred)
plot(roc(test$income,pred))
cat("AUC",auc,"\n")


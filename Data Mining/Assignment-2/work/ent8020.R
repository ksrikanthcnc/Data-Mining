#--------------------------------------------------------------------
#library(doMC)
#library(doParallel)
#registerDoMC(cores=3)

while (dev.cur() > 1) dev.off()
rm(list = ls())
data <- read_csv("finalfact.csv")
data <- data[1:1000,]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
#data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)
#View(data)
#summary(data)
#head(data)
#str(data)
#--------------------------------------------------------------------
#10-fold
fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE
                                          ,savePredictions = TRUE
)
ratio <- trainControl(number = 5,
                      verboseIter = TRUE
                                           ,savePredictions = TRUE,
)

f <- as.formula(paste(" ~", paste(names(data), collapse = " + ")))
m <- model.matrix( f,data = data)
data <- as.data.frame(m)
#data$incomeLow <-  as.factor(data$incomeLow)
#data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)

#80:20 with sampling
intrain <- createDataPartition(y = data$income, p = 0.8, list = FALSE)
train <- data[intrain,]
test <- data[-intrain,]
#any(is.na(data) == TRUE)
#-----------------------------------------------------------------
#models
#info,gini
#tree_ent <- rpart(income~., train_ratio, method = "class", minsplit = 2, minbucket = 1,
#                  parms = list(split = "gini"))
#rpart.plot(tree_ent)

#entropy  
cat("--------------------MODELS------------------\n")
cat("~~~~~~~~~~Entropy~~~~~~~~\n")
ent_ratio <- train(incomeLow~., data = train, method = "rpart",
#                                    warning(T),
                   parms = list(split = "information"),
                   trControl = ratio,
                   tuneLength = 10)
prp(ent_ratio$finalModel, box.palette = "Reds")
cat("\nEntropy with 80:20\n")

model<-ent_ratio

predicted <- predict(model, test)
View(predicted)
View(pred)
pred=ifelse((predicted<0.5), 0,1)
#pred=ifelse((predicted == "Low"), 1,2)
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
pred <- predict(model, test, type = 'raw')
auc<-auc(test$income,pred)
plot(roc(test$income,pred))
cat("AUC",auc,"\n")


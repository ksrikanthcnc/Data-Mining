fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(number = 5,
                      verboseIter = TRUE,
                      savePredictions = TRUE,
)

#cat("--------------------MODELS------------------\n")
data <- read_csv("finalfact.csv")
data <- data[1:1000,]

#income <- ifelse( data$income == "Low", 0,1)
#data <- data[,-14]
#data <- data.frame(data, income)
###data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
f <- as.formula(paste(" ~", paste(names(data), collapse = " + ")))
m <- model.matrix( f,data = data)
d <- as.data.frame(m)

#d[sapply(d, is.numeric)] <- lapply(d[sapply(d, is.numeric)], scale)
#d <- d[,-1]
#d$`(Intercept)` <- 1
#inc = ifelse( d$`(Intercept)` == NA, 1,NA)
#inc
#d = d[,-1]
#d = data.frame(d, income)
d <- data

intrain <- createDataPartition(y = d$income, p = 0.8, list = FALSE)
train <- d[intrain,]
test <- d[-intrain,]
#View(d)

nn <- train(income~., data = train,
            method = "nnet",
            trControl = fold,
            tuneLength = 10)
plot.nnet(nn)
predicted <-predict(nn, test)
#summary(predicted)
cat("Confusion Matrix","\n")
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
#t <- test
#p <- predicted
test$income <- ifelse( test$income == "Low", 1,2)
predicted <- ifelse( predicted == "Low", 1,2)
auc<-auc(test$income,predicted)
plot(roc(test$income,predicted))
cat("AUC",auc,"\n")



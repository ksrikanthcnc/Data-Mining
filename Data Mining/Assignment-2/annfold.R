data <- read_csv("finalfact.csv")
data <- data[1:1000,]

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], scale)
#View(data)
f <- as.formula(paste(" ~", paste(names(data), collapse = " + ")))
m <- model.matrix( f,data = data)
d <- as.data.frame(m)
#View(d$`(Intercept)`)
#View(d)
d$`(Intercept)` <- NULL
intrain <- createDataPartition(y = d$income, p = 0.8, list = FALSE)
train <- d[intrain,]
test <- d[-intrain,]
#View(d)
n <- names(d)
f <- as.formula(paste("incomeLow~", paste(sprintf("`%s`", n[!n %in% "incomeLow"]), collapse="+")))

folds <- createFolds(y = data$income,k =10)

maxi=0

for (i in 1:10) {
ind <- folds[[1]]
test <- d[ind,]
train <- d[-ind,]  
ann <- neuralnet( f,data=train,hidden=c(5),
                  lifesign = 'full',
                  threshold=0.1,rep=10)

predicted_ <- compute(ann, test[,1:length(test)-1], rep = 1)
predicted <- predicted_
predicted <- ifelse( predicted_$net.result < 0, 0,1)
conf <- table(predicted,test$income)
auc<-auc(test$income,predicted)
if(auc > maxi){
  annmax <- ann
  max = auc
}

}
plot(annmax)
plot(ann)
ann <- annamx

predicted_ <- compute(ann, test[,1:length(test)-1], rep = 1)
#summary(predicted_$net.result)
predicted <- predicted_
predicted <- ifelse( predicted_$net.result < 0, 0,1)
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

auc<-auc(test$income,predicted)
plot(roc(test$income,predicted))
cat("AUC",auc,"\n")




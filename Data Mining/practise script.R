iris <- read.csv("/home/srikanth/workspace/NitC/Data Mining/R/iris.csv")
View(iris)
str(iris)
dim(iris)
names(iris)
attributes(iris)
iris[1:10, ]
randsamp <- sample(1:nrow(iris),10)
randsamp
iris[randsamp, ]
iris[1:10, 1]
summary(iris)
quantile(iris$sepal_length)
var(iris$sepal_length)
hist(iris$sepal_length)
pie(table(iris$species))
cov(iris$sepal_length,iris$sepal_width)
cor(iris$sepal_length,iris$petal_length)
boxplot(sepal_length ~ species, data = iris, xlab = "species", ylab = "sepal_length")
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainData <- iris[ind == 1,]
testData <- iris[ind == 2,]
summary(trainData)
library(party)
myFormula <- species ~ sepal_length + sepal_width + petal_length + petal_width
iris_ctree <- ctree (myFormula, data=trainData)
table(predict(iris_ctree), trainData$species)
plot(iris_ctree)  
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$species)
irisnoclass <- iris
irisnoclass$species <- NULL
(kmeans.result <- kmeans(irisnoclass, 3))
table(iris$species, kmeans.result$cluster)
plot(irisnoclass[c("sepal_length", "sepal_width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("sepal_length", "sepal_width")], col = 1:3, pch = 8, cex = 2)

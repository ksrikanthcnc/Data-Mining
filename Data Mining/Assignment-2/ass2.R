rm(list = ls())

library(ISLR)
library(tree)
adult_data <- read_csv("adult-data.csv")
detach(adult_data)
attach(adult_data)
inc = ifelse( income == "<=50000", "Low", "High")
#names(adult_data)
adult_data = data.frame(adult_data, inc)
adult_data = adult_data[,-15]
set.seed(1234)
train = sample(1:nrow(adult_data), nrow(adult_data)/2)
test = -train
training_data = adult_data[train,]
testing_data = adult_data[test,]
testing_inc=inc[test]

tree_model=tree(inc~.,training_data)
#View(tree_model)
library(rpart.plot)
rpart.plot(tree_model)
text(tree_model,pretty=0)

tree_pred=predict(tree_model,testing_data,type="class")
mean(tree_pred != testing_data)



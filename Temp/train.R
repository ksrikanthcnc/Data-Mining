#working
#ENTROPY information gain https://rstudio-pubs-static.s3.amazonaws.com/202050_60f5c6dd1b884ba3ac29f1aef5c848bb.html
rm(list = ls())

library(readr)
adult_data <- read_csv("adult-data.csv")
#summary(adult_data)
#str(adult_data)
set.seed(1234)

income = ifelse( adult_data$income == "<=50000", "Low", "High")
#names(adult_data)
adult_data = adult_data[,-15]
adult_data = data.frame(adult_data, income)

#library(DT)
library(caret)
intrain <- createDataPartition(y = adult_data$inc, p = 0.8, list = FALSE)
#adult_data_small <- adult_data[intrain,]
#adult_data_small <- adult_data
#datatable(adult_data_small, filter="top")

# Shuffling the dataset
n <- nrow(adult_data)
ads <- adult_data[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.8 * n)
train <- ads[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
test <- ads[test_indices, ]

library(rpart)
library(rpart.plot)
tree_ent <- rpart(income~., train, method = "class", minsplit = 2, minbucket = 1,
                  parms = list(split = "information"))
rpart.plot(tree_ent)

#typeof(inc)
#View(inc)
pred_test <- predict(tree_ent, test, type = "class")
conf <- table(test$income, pred_test)
conf
acc <- sum(diag(conf))/sum(conf)
acc

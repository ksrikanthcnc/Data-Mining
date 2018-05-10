network$result.matrix
head(network$generalized.weights[[1]])
plot(network)
data1 <- data
data1$income<- NULL
head(data1)
data1 <- read_csv("data.csv")
kmeans(data1,3,trace=TRUE)

results <- kmeans(data1,2)
results
D<- daisy(finaldata.features)

plot(silhouette(results$cluster, D), col=1:2, border=NA)
commands.txt
Displaying commands.txt.



rm(list = ls())

library(readr)
set.seed(1234)
adult_data <- read_csv("data.csv")
adult_data = adult_data[,-14]

adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.numeric)
adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.numeric)], scale)

kadult <- kmeans(adult_data, center = 2, nstart = 20)
kadult <- kmeans(adult_data$sex, center = 1, nstart = 20)
kadult


 D<- daisy()
?daisy


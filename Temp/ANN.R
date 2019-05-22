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

library(neuralnet)
income = ifelse( adult_data$income == "<=50000", 0,1)
adult_data = data.frame(adult_data, income)
adult_data = adult_data[,-15]

m <- model.matrix(income~age+workclass+fnlwgt+education+`education-num`+`marital-status`+occupation+relationship+race+sex+`capital-gain`+`capital-loss`+`hours-per-week`+`native-country`, data = training)
adult_data[] <- lapply( adult_data, factor)
str(adult_data)
a <- as.numeric(levels(adult_data)[adult_data])
nn <- neuralnet(income~age+workclass+fnlwgt+education+`education-num`+`marital-status`+occupation+relationship+race+sex+`capital-gain`+`capital-loss`+`hours-per-week`+`native-country`, data = adult_data, hidden = 2)
nn$result.matrix
View(m)
any(is.na(m))
plot(nn)

library(neuralnet)
data <- adult_data
finaldata <- adult_data
ind <- sample(2, nrow(adult_data), replace = TRUE, prob=c(0.7, 0.3))
trainset = finaldata[ind == 1,]
testset = finaldata[ind == 2,]
trainset$yes= trainset$income == "yes"
trainset$no= trainset$income == "no"

n <- names(adult_data)
f <- as.formula(paste("income ~", paste(n[!n %in% "y"], collapse = " + ")))
f
network = neuralnet(f, trainset, hidden=2)

m <- model.matrix( f,  data = adult_data )
head(m)
library(neuralnet)
r <- neuralnet( 
f,  data=m, hidden=10, threshold=0.01
)

network
network$result.matrix



n <- names(training)
f <- as.formula(paste("income ~", paste(n[!n %in% "income"], collapse = " + ")))
nn <- neuralnet(f,data=training,hidden=c(5,3),linear.output=T)
adult_data
as.data.frame(adult_data)
as.matrix(adult_data)

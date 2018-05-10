rm(list = ls())
library(readr)
adult.train <- read_csv("adult-data.csv")
set.seed(1234)
columns<-c('age','workclass','fnlwgt','education','EducationNo','MaritalStatus','occupation','relationship','race','sex','CapitalGain','CapitalLoss','HoursPerWeek','NativeCountry','salary')
colnames(adult.train)<-columns
adult.train<-subset(adult.train,select =-c(fnlwgt,EducationNo))
train<-adult.train

#To find out corrplot  after converting it into factor variables
#First convert variables in the train dataset into factor datatype
conversion<-c(2:8,12,13)
train[,conversion]<-lapply(train[,conversion],factor)
train2<-train
#Convert factor variables into numeric datatype to examine corrplot
train2[,conversion]<-lapply(train2[,conversion],as.numeric)
library(corrplot)
train_corrplot<-corrplot(cor(train2,use ="complete.obs" ),method="number",bg="black")


#Recoding factor levels in categorical variables
#workclass
library(car)
train$workclass<-recode(train$workclass,'c(" Never-worked"," Without-pay")="Unemployed"')

#education
train$education<-recode(train$education,'c(" Preschool"," 1st-4th"," 7th-8th", " 9th", " 10th"," 5th-6th"," 11th"," 12th")="SchoolLevel"')
train$education<-recode(train$education,'c(" Doctorate"," Masters")="HigherEdu"')

#Marital Status
train$MaritalStatus<-recode(train$MaritalStatus,'c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent")="Married"')
train$MaritalStatus<-recode(train$MaritalStatus,'c(" Divorced"," Separated")="NotTogether"')

#Ocuupation
train$occupation<-recode(train$occupation,'c(" Armed-Forces"," Other-service")="Other-Services"')
train$occupation<-recode(train$occupation,'c(" Priv-house-serv"," Handlers-cleaners")=" Handlers-cleaners"')
train$occupation<-recode(train$occupation,'c(" Farming-fishing"," Handlers-cleaners"," Transport-moving"," Craft-repair"," Machine-op-inspct")="BlueCollar"')


#Race
train$race<-recode(train$race,"c(' Other',' Asian-Pac-Islander',' Amer-Indian-Eskimo')='Others'")


#Native Country
train$NativeCountry<-recode(train$NativeCountry,'c(" Scotland"," Ireland"," England")="Great-Britain"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Cambodia"," Laos"," Vietnam"," Hong"," Philippines"," Taiwan"," Thailand")="South-East-Asia"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Trinadad&Tobago"," Puerto-Rico"," Jamaica"," Dominican-Republic"," Haiti"," Cuba")="Caribbean-Islands"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Honduras"," El-Salvador"," Guatemala"," Nicaragua")="Central-America"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Columbia"," Ecuador"," Peru"," South")="South-America"')
train$NativeCountry<-recode(train$NativeCountry,'c(" China"," Japan"," India")="Asia"')
train$NativeCountry<-recode(train$NativeCountry,'c(" Outlying-US(Guam-USVI-etc)"," United-States" )="United-States"')
train$NativeCountry<-recode(train$NativeCountry,'c(" France"," Germany"," Holand-Netherlands"," Hungary"," Italy"," Poland"," Portugal"," Yugoslavia"," Greece")="Eurasia"')


#Continuous variables
train<-subset(train,train$age<75)#First only include the data of people who's age isn;t above 74 in the dataset because that's the maximum age in the testing dataset and 99% of people's age in the training dataset is less than or equal to 74
train$age<-NULL #Then remove age variable as 35% of the data present in the test dataset is less than that of the minimum value present in the train dataset




#Test dataset
#Data cleaning
#Deleting education no and fnlwgt
test<-adult.train
test[,conversion]<-lapply(test[,conversion],factor)

#Recoding categorical variables
#workclass
test$workclass<-recode(test$workclass,'c(" Never-worked"," Without-pay")="Unemployed"')

#Education
test$education<-recode(test$education,'c(" Preschool"," 1st-4th"," 7th-8th", " 9th", " 10th"," 5th-6th"," 11th"," 12th")="SchoolLevel"')
test$education<-recode(test$education,'c(" Doctorate"," Masters")="HigherEdu"')

#Marital Status
test$MaritalStatus<-recode(test$MaritalStatus,'c(" Married-AF-spouse"," Married-civ-spouse"," Married-spouse-absent")="Married"')
test$MaritalStatus<-recode(test$MaritalStatus,'c(" Divorced"," Separated")="NotTogether"')

#Occupation
test$occupation<-recode(test$occupation,'c(" Armed-Forces"," Other-service")="Other-Services"')
test$occupation<-recode(test$occupation,'c(" Priv-house-serv"," Handlers-cleaners")=" Handlers-cleaners"')
test$occupation<-recode(test$occupation,'c(" Farming-fishing"," Handlers-cleaners"," Transport-moving"," Craft-repair"," Machine-op-inspct" )="BlueCollar"')

#Race
test$race<-recode(test$race,"c(' Other',' Asian-Pac-Islander',' Amer-Indian-Eskimo')='Others'")

#Native Country
test$NativeCountry<-recode(test$NativeCountry,'c(" Scotland"," Ireland"," England")="Great-Britain"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Cambodia"," Laos"," Vietnam"," Hong"," Philippines"," Taiwan"," Thailand")="South-East-Asia"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Trinadad&Tobago"," Puerto-Rico"," Jamaica"," Dominican-Republic"," Haiti"," Cuba")="Caribbean-Islands"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Honduras"," El-Salvador"," Guatemala"," Nicaragua")="Central-America"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Columbia"," Ecuador"," Peru"," South")="South-America"')
test$NativeCountry<-recode(test$NativeCountry,'c(" China"," Japan"," India")="Asia"')
test$NativeCountry<-recode(test$NativeCountry,'c(" Outlying-US(Guam-USVI-etc)"," United-States" )="United-States"')
test$NativeCountry<-recode(test$NativeCountry,'c(" France"," Germany"," Holand-Netherlands"," Hungary"," Italy"," Poland"," Portugal"," Yugoslavia"," Greece")="Eurasia"')

#Continuous variables
test$age<-NULL

test

any(is.na(test))
a <- model.matrix(~.+0, data=adult.train)
kadult <- kmeans(a, 2, nstart = 20)
kadult

library(cluster)
library(HSAUR)
km    <- kmeans(a,2)
dissE <- daisy(a) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)
data <- read_csv("data.csv")


data$type_employer = as.character(data$type_employer) 
data$occupation = as.character(data$occupation) 
data$country = as.character(data$country) 
data$race = as.character(data$race) 
data$marital = as.character(data$marital) 
data$marital[data$marital=="Never-married"] = "Never-Married" 
data$marital[data$marital=="Married-AF-spouse"] = "Married" 
data$marital[data$marital=="Married-civ-spouse"] = "Married" 
data$marital[data$marital=="Married-spouse-absent"] = "Not-Married" 
data$marital[data$marital=="Separated"] = "Not-Married" 
data$marital[data$marital=="Divorced"] = "Not-Married" 
data$marital[data$marital=="Widowed"] = "Widowed" 
data$country[data$country=="Cambodia"] = "SE-Asia" # blocking Country of Origin 
data$country[data$country=="Canada"] = "British-Commonwealth" 
data$country[data$country=="China"] = "China" 
data$country[data$country=="Columbia"] = "South-America"
data$country[data$country=="Cuba"] = "Other"
data$country[data$country=="Dominican-Republic"] = "Latin-America"
data$country[data$country=="Ecuador"] = "South-America"
data$country[data$country=="El-Salvador"] = "South-America"
data$country[data$country=="England"] = "British-Commonwealth"
data$country[data$country=="France"] = "Euro_1"
data$country[data$country=="Germany"] = "Euro_1"
data$country[data$country=="Greece"] = "Euro_2"
data$country[data$country=="Guatemala"] = "Latin-America"
data$country[data$country=="Haiti"] = "Latin-America"
data$country[data$country=="Holand-Netherlands"] = "Euro_1"
data$country[data$country=="Honduras"] = "Latin-America"
data$country[data$country=="Hong"] = "China"
data$country[data$country=="Hungary"] = "Euro_2"
data$country[data$country=="India"] = "British-Commonwealth"
data$country[data$country=="Iran"] = "Other"
data$country[data$country=="Ireland"] = "British-Commonwealth"
data$country[data$country=="Italy"] = "Euro_1"
data$country[data$country=="Jamaica"] = "Latin-America"
data$country[data$country=="Japan"] = "Other"
data$country[data$country=="Laos"] = "SE-Asia"
data$country[data$country=="Mexico"] = "Latin-America"
data$country[data$country=="Nicaragua"] = "Latin-America"
data$country[data$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
data$country[data$country=="Peru"] = "South-America"
data$country[data$country=="Philippines"] = "SE-Asia"
data$country[data$country=="Poland"] = "Euro_2"
data$country[data$country=="Portugal"] = "Euro_2"
data$country[data$country=="Puerto-Rico"] = "Latin-America"
data$country[data$country=="Scotland"] = "British-Commonwealth"
data$country[data$country=="South"] = "Euro_2"
data$country[data$country=="Taiwan"] = "China"
data$country[data$country=="Thailand"] = "SE-Asia"
data$country[data$country=="Trinadad&Tobago"] = "Latin-America"
data$country[data$country=="United-States"] = "United-States"
data$country[data$country=="Vietnam"] = "SE-Asia"
data$country[data$country=="Yugoslavia"] = "Euro_2"
data$type_employer = gsub("^Federal-gov","Federal-Govt",data$type_employer)
data$type_employer = gsub("^Local-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^State-gov","Other-Govt",data$type_employer)
data$type_employer = gsub("^Private","Private",data$type_employer)
data$type_employer = gsub("^Self-emp-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Self-emp-not-inc","Self-Employed",data$type_employer)
data$type_employer = gsub("^Without-pay","Not-Working",data$type_employer)
data$type_employer = gsub("^Never-worked","Not-Working",data$type_employer)
data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar",data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar",data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar",data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar",data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional",data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations",data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations",data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar",data$occupation)
data$race[data$race=="White"] = "White"
data$race[data$race=="Black"] = "Black"
data$race[data$race=="Amer-Indian-Eskimo"] = "Amer-Indian"
data$race[data$race=="Asian-Pac-Islander"] = "Asian"
data$race[data$race=="Other"] = "Other"

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.numeric)], scale)

data[["capital_gain"]] <- ordered(cut(data$capital_gain,c(-Inf, 0, median(data[["capital_gain"]][data[["capital_gain"]] >0]), Inf)), labels = c("None", "Low", "High"))
data[["capital_loss"]] <- ordered(cut(data$capital_loss,c(-Inf, 0, median(data[["capital_loss"]][data[["capital_loss"]] >0]), Inf)), labels = c("None", "Low", "High"))

summary(data)
head(data)

library(nnet)
a = nnet(income~., data=train,size=10,maxit=150,decay=.001)
plot.nnet(a)
table(data$val$income,predict(a,newdata=data$val,type="class"))







data <- read_csv("data.csv")
db.adult <- data
library(dplyr)


Asia_East <- c(" Cambodia", " China", " Hong", " Laos", " Thailand",
               " Japan", " Taiwan", " Vietnam")

Asia_Central <- c(" India", " Iran")

Central_America <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", 
                     " Puerto-Rico",  " Dominican-Republic", " El-Salvador", 
                     " Haiti", " Honduras", " Mexico", " Trinadad&Tobago")

South_America <- c(" Ecuador", " Peru", " Columbia")


Europe_West <- c(" England", " Germany", " Holand-Netherlands", " Ireland", 
                 " France", " Greece", " Italy", " Portugal", " Scotland")

Europe_East <- c(" Poland", " Yugoslavia", " Hungary")
db.adult$
db.adult <- mutate(db.adult, 
                   native_region = ifelse(country %in% Asia_East, " East-Asia",
                                          ifelse(country %in% Asia_Central, " Central-Asia",
                                                 ifelse(country %in% Central_America, " Central-America",
                                                        ifelse(country %in% South_America, " South-America",
                                                               ifelse(country %in% Europe_West, " Europe-West",
                                                                      ifelse(country %in% Europe_East, " Europe-East",
                                                                             ifelse(country == " United-States", " United-States", 
                                                                                    " Outlying-US" ))))))))

db.adult <- mutate(db.adult, 
                   cap_gain = ifelse(db.adult$capital_gain < 3464, " Low",
                                     ifelse(db.adult$capital_gain >= 3464 & 
                                              db.adult$capital_gain <= 14080, " Medium", " High")))


db.adult$cap_gain <- factor(db.adult$cap_gain,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))


db.adult <- mutate(db.adult, 
                   cap_loss = ifelse(db.adult$capital_loss < 1672, " Low",
                                     ifelse(db.adult$capital_loss >= 1672 & 
                                              db.adult$capital_loss <= 1977, " Medium", " High")))


db.adult$cap_loss <- factor(db.adult$cap_loss,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))

a <- data
a$age[a$age>=17 & a$age<29] <- 1
a$age[a$age>=29 & a$age<38] <- 2
a$age[a$age>=38 & a$age<48] <- 3
a$age[a$age>=48 & a$age<=90] <- 4

a$hrperweek[a$hrperweek>=0 & a$hrperweek <40] <- 1
a$hrperweek[a$hrperweek>=40 & a$hrperweek <45] <- 2
a$hrperweek[a$hrperweek>=45 & a$hrperweek <60] <- 3
a$hrperweek[a$hrperweek>=60 & a$hrperweek <80] <- 4
a$hrperweek[a$hrperweek>=80 & a$hrperweek <100] <- 5

a$capitalgain[a$capitalgain>=0 & a$capitalgain<=114] <- 1
a$capitalgain[a$capitalgain>114 & a$capitalgain<=3464] <- 2
a$capitalgain[a$capitalgain>3464 & a$capitalgain<=7298] <- 3
a$capitalgain[a$capitalgain>7298 & a$capitalgain<=14084] <- 4
a$capitalgain[a$capitalgain>14084 & a$capitalgain<=99999] <- 5

a$capitalloss[a$capitalloss>=0 & a$capitalloss<=155] <- 1
a$capitalloss[a$capitalloss>155 & a$capitalloss<=1672] <- 2
a$capitalloss[a$capitalloss>1672 & a$capitalloss<=1887] <- 3
a$capitalloss[a$capitalloss>1887 & a$capitalloss<=1977] <- 4
a$capitalloss[a$capitalloss>1977 & a$capitalloss<=4356] <- 5

a$educ[a$educ>0 & a$educ<=8] <- 1
a$educ[a$educ>8 & a$educ<=10] <- 2
a$educ[a$educ>10 & a$educ<=13] <- 3
a$educ[a$educ>13 & a$educ<=16] <- 4
summary(a)
write.csv(a,file = "output.csv",)
?write.csv

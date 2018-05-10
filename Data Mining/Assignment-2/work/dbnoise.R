data <- read_csv("finalnum.csv")
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
#d <- data

d$income <- NULL
d$`(Intercept)` <- NULL

#str(d)
library(cluster)

#k1<-kmeans(x = d,centers = 1,trace = T)
#k2<-kmeans(x = d,centers = 2,trace = T)
#k3<-kmeans(x = d,centers = 3,trace = T)

c1<-clara(x = d,k = 1)
c2<-clara(x = d,k = 2)
c3<-clara(x = d,k = 3)

c2[["clusinfo"]]

library(dbscan)
db<-dbscan(d,eps=661673,minPts = 6093)
db

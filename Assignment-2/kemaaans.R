fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(number = 5,
                      verboseIter = TRUE,
                      savePredictions = TRUE,
)

#cat("--------------------MODELS------------------\n")
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

View(d)
d$income <- NULL
d$`(Intercept)` <- NULL

#str(d)
print('totss')
print('tot.withinss')
k1 <- kmeans(x = d,
             trace = T,
             centers =  1
)
View(d)
#k1
k1$totss
k1$tot.withinss

k2 <- kmeans(x = d,
             trace = T,
             centers =  2
)
#k2
k2$totss
k2$tot.withinss

k3 <- kmeans(x = d,
             trace = T,
             centers = 3
)
#k3
k3$totss
k3$tot.withinss
View(k3)
k3$withinss

silh

print('clusinfo')
print('silinfo')
library(cluster)
p <- pam(x = d,k = 1)
p$clusinfo
p$silinfo
p <- pam(x = d,k = 2)
p$clusinfo
p$silinfo
View(p)
p <- pam(x = d,k = 3,
         trace.lev = 2)
p$clusinfo
p$silinfo

summary(p)

silhouette(p)
View(silhouette(p))

pam()
c<-clara(x = d,k = 1)
View(c)
c
View(data)
?fanny
k1<-kmeans(x = d,centers = 1,trace = T)
k2<-kmeans(x = d,centers = 2,trace = T)
k3<-kmeans(x = d,centers = 3,trace = T)

f1<-fanny(x = d,k = 1,trace.lev = 2)
f2<-fanny(x = d,k = 2,trace.lev = 2)
f3<-fanny(x = d,k = 3,trace.lev = 2)
f1[["silinfo"]][["clus.avg.widths"]]
f1[["silinfo"]][["avg.width"]]
f2[["silinfo"]][["clus.avg.widths"]]
f2[["silinfo"]][["avg.width"]]
f3[["silinfo"]][["clus.avg.widths"]]
f3[["silinfo"]][["avg.width"]]
View(f3)
p1<-pam(x = d,k = 1,trace.lev = 2)
p2<-pam(x = d,k = 2,trace.lev = 2)
p3<-pam(x = d,k = 3,trace.lev = 2)
p1[["silinfo"]][["clus.avg.widths"]]
p1[["silinfo"]][["avg.width"]]
p2[["silinfo"]][["clus.avg.widths"]]
p2[["silinfo"]][["avg.width"]]
p3[["silinfo"]][["clus.avg.widths"]]
p3[["silinfo"]][["avg.width"]]

c1<-clara(x = d,k = 1)
c2<-clara(x = d,k = 2)
c3<-clara(x = d,k = 3)
View(c2)
View(p2)
View(f2)
View(k2)
library(dbscan)
db<-dbscan(d,eps=850666,minPts = 1000)
db
850666

c[["clusinfo"]]
andhulo eps=max_diss avg anukunta
minpts=minimum in size
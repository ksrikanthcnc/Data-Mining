fold <- trainControl(method="repeatedcv", number = 10,repeats = 1,
                     verboseIter = TRUE,
                     savePredictions = TRUE
)
ratio <- trainControl(number = 5,
                      verboseIter = TRUE,
                      savePredictions = TRUE,
)

#cat("--------------------MODELS------------------\n")
data <- read_csv("finalfact.csv")
#data <- data[1:1000,]

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

#View(d)
d$income <- NULL
d$`(Intercept)` <- NULL


  f1<-fanny(x = d,k = 1)
  f2<-fanny(x = d,k = 2)
  f3<-fanny(x = d,k = 3)
  print("fanny")
  f1[["silinfo"]][["clus.avg.widths"]]
  f1[["silinfo"]][["avg.width"]]
  f2[["silinfo"]][["clus.avg.widths"]]
  f2[["silinfo"]][["avg.width"]]
  f3[["silinfo"]][["clus.avg.widths"]]
  f3[["silinfo"]][["avg.width"]]
  #View(f3)
  print("pam")
  p1<-pam(x = d,k = 1)
  p2<-pam(x = d,k = 2)
  p3<-pam(x = d,k = 3)
  p1[["silinfo"]][["clus.avg.widths"]]
  p1[["silinfo"]][["avg.width"]]
  p2[["silinfo"]][["clus.avg.widths"]]
  p2[["silinfo"]][["avg.width"]]
  p3[["silinfo"]][["clus.avg.widths"]]
  p3[["silinfo"]][["avg.width"]]
  c1<-clara(x = d,k = 1)
  c2<-clara(x = d,k = 2)
  c3<-clara(x = d,k = 3)
  print("clara")
  c1[["silinfo"]][["clus.avg.widths"]]
  c1[["silinfo"]][["avg.width"]]
  c2[["silinfo"]][["clus.avg.widths"]]
  c2[["silinfo"]][["avg.width"]]
  c3[["silinfo"]][["clus.avg.widths"]]
  c3[["silinfo"]][["avg.width"]]

  db<-dbscan::dbscan(d,eps=444227,minPts = 490)
db$cluster
dbscan::kNNdistplot(d, k =  3)

clara(x = d,)  
library(fpc)
?fpc    
  distcritmulti(x = d,clustering = db$cluster)

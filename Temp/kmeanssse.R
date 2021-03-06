
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

#View(d)
d$income <- NULL
d$`(Intercept)` <- NULL

#str(d)
#print('totss')
#print('tot.withinss')
k1 <- kmeans(x = d,
             trace = T,
             centers =  1
)
k1
k2 <- kmeans(x = d,
             trace = T,
             centers =  2
)
k2
k3 <- kmeans(x = d,
             trace = T,
             centers = 3
)
k3


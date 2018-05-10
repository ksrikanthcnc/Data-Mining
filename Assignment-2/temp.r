d <- data[,-14]
d <- m
result<-daisy(d, 3,metric = c("euclidean"))
plot(silhouette(result$cluster, D), col=1:2, border=NA)

k<-kmeans(d,3)
str(d)

plot(k$centers)
adult_data<-read_csv("data.csv")
adult_data[sapply(adult_data, is.character)] <- lapply(adult_data[sapply(adult_data, is.character)], as.factor)
adult_data<-as.data.frame(adult_data[1:1000,])
k<-kproto(adult_data,2)
k$centers
plot(k$centers$age,k$centers$income)
plot(d$age,d$income)


clprofiles(k,d)

plot(k$data)
head(k)
p <- pam(result,3,diss=TRUE,trace.lev = 2)
plot(p)
f <- fanny(result, 2,diss=T, trace.lev = 2,memb.exp = 1.1)
plot(f)
a <- agnes(result,diss=T,trace.lev = 2,keep.data = F,keep.diss = F)
plot(a)
d <- diana(result, diss=T)
plot(d)

plot(silhouette(results$cluster, D), col=1:2, border=NA)
result

library(dbscan)

z <- frNN(result,eps = 2)
x<-dbscan(z)
plot(x$minPts)
x$cluster





gower_dist <- daisy(data[,-14],3,metric="gower")
gower_mat <- as.matrix(gower_dist)

sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
library(Rtsne)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)













set.seed(1234)

data <- read_csv("data.csv")
data <- data[1:1000,]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.numeric)], scale)


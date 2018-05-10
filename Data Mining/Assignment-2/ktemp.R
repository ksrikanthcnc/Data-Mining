km1 = kmeans(m, 2, nstart=100)

plot(m, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)
km1$
m
km1



mydata <- m[1:1000,]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


km2 = kmeans(m, 6, nstart=100)
plot(m, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
km2
head(km2)


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(m)

km2$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster)

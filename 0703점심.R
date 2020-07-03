((8-4)^2+(4-4)^2)^0.5

c <- c(3,4,1,5,7,9,5,4,6,8,4,5,9,8,7,8,6,7,2,1)
row <- c("A","B","C","D","E","F","G","H","I","J")
col <- c("X","Y")
data <- matrix( c, nrow= 10, ncol=2, byrow=TRUE, dimnames=list(row,col))
data


plot(data)

install.packages("stats")
library(stats)

length(data)
k = sqrt(length(data)/2)
k
km <- kmeans(data,2)
km
km$centers


plot(round(km$center), col=km$center, pch=22,bg=km$center, xlim=range(0:10),ylim=range(0:10))
par(new=T)
plot( data, col=km$cluster+1,xlim=range(0:10), ylim=range(0:10) )


install.packages("factoextra")
library(factoextra)

km <- kmeans(data,2)

fviz_cluster( km, data = data, stand=F)
km
km$cluster
km$totss
km$withinss
km$tot.withinss
km$betweenss
km$size
km$iter
km$ifault




c <- c(10,9,1,4,10,1,7,10,3,10,1,1,6,7)
row <- c("APPLE","BACON","BANANA","CARROT","SAL","CHEESE","TOMATO")
col <- c("X","Y")
data <- matrix( c, nrow= 7, ncol=2, byrow=TRUE, dimnames=list(row,col))
data

km <- kmeans(data, 3)
km
cbind(data, km$cluster)


plot(round(km$center), col=km$center, pch=22, bg=km$center, xlim=range(0:10),ylim=range(0:10))
par(new=T) # 그래프 겹치기
plot( data, col=km$cluster+1, xlim=range(0:10), ylim=range(0:10), pch=22, bg=km$cluster+1 )
fviz_cluster( km, data = data, stand=F)




#--------
  
academy <- read.csv("d:\\data\\csv\\academy.csv")
head(academy)
aca_t <- academy[3:4]
head(aca_t)
km <- kmeans(aca_t,4)

km
plot(round(km$center), col=km$center, pch=22, bg=km$center, xlim=range(30:100),ylim=range(30:100))
par(new=T) # 그래프 겹치기
plot( aca_t, col=km$cluster+1, xlim=range(30:100), ylim=range(30:100), pch=22, bg=km$cluster+1 )
fviz_cluster( km, data = aca_t, stand=F)

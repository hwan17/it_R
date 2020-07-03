myknn <- function(train, test, cl, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    temp <- t(t(train)-c(t(test[i,])))^2
    temp <- sqrt(rowSums(temp))
    table <- data.frame(train, kind=cl, d=temp)
    table$rnk <- rank(table$d, ties.method = 'min')
    top <- table[table$rnk <= k,'kind']
    pred <- append(pred, names(which.max(table(top))))
  }
  return(pred)
}




wbcd <- read.csv("d:\\data\\csv\\wisc_bc_data.csv", header=T, stringsAsFactors=FALSE)



wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))



wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]



wbcd2 <- wbcd_shuffle[-1]



normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x)) )
}

wbcd_n <- as.data.frame(lapply(wbcd2[2:31],normalize))



train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]



wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]


result1 <-myknn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_label, k=21)
result1

library(gmodels)
CrossTable(wbcd_test_label,result1)



#--------------------------------------------------------------------



a
b <- matrix(c(3,4,3,2,2,1,3,2,3,2,3,2,1,2,2,1,2,1,4,3),ncol = 4, byrow = T)
b
b <- matrix(c(3,4,3,2),ncol=4,byrow = T)

a-b
b
a-b
t(a)-c(t(b))


d = c(1,2,3,4,5,6)
e = c(1,2,3)
d-e
length(d)
length(e)
length(t(a))
length(t(b))
str(t(a))
dim(a)

class(d)
class(t(a))
class(t(b))


a[1,]= a[1,]-b
b
a[1,]


a <- matrix(c(4,3,2,5,2,4,2,9,8,2,3,3,9,3,2,4,3,2,1,2),ncol = 4 , byrow = T)
b <- matrix(c(3,4,3,2),ncol=4,byrow = T)

nrow(a)
for (i in 1:nrow(a)){
  a[i,] = a[i,]-b 
}
a

for (i in 1:nrow(a)){
  for (j in 1:ncol(a))
    a[i,j] = a[i,j]-b[j] 
}
a



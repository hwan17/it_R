# 유클리드 거리 공식 R로 구현

a = c(2,4)
b = c(5,6)
sqrt((a[1]-b[1])^2 + (a[2]-b[2])^2)

# 3차원에서 두점 사이의 거리 구하기

a = c(0,3,2)
b = c(2,0,0)
sqrt( sum((a-b)^2) )

# 두점사이의 거리를 구하는 함수 생성
distance <- function(a,b) {
  return ( sqrt( sum( (a-b)^2 ) ) )
}
distance(a,b)

# 216.여러 개의 지점과 c(4,4) 지점과의 거리를 각각 비교

a = c(1,5)
c = c(4,4)

x <- c(1,2,4,5,6,1)
y <- c(5,6,5,2,3,7)

x[2]
y[2]

temp <- c()
for ( i in 1:6){
  temp[i] <- ( distance( c(x[i],y[i]), c(4,4) ) )
}
temp


temp2 <- c()
for ( i in 1:6){
  temp2 <- append(temp2, distance( c(x[i],y[i]), c(4,4) ) )
}
temp2

#217
min(temp)


#218

fruits <- data.frame('재료'=c('사과','베이컨','바나나','당근','셀러리','치즈'),
                     '단맛'=c(10,1,10,7,3,1),
                     '아삭한맛'=c(9,4,1,10,10,1),
                     '음식종류'=c('과일','단백질','과일','채소','채소','단백질'))

fruits
토마토 = c(6,4)

dd <- c()
for ( i in 1:6){
  dd <- append(dd, distance( c(fruits$단맛[i],fruits$아삭한맛[i]), 토마토 ) )
}
dd
min(dd)

fruits$dist <- dd
fruits

fruits [ fruits$dist == min(fruits$dist), "음식종류"]


# fruits의 파생변수인 dist 를 이용해서 순위 파생변수 추가
fruits$rnk <- rank(fruits$dist)
fruits

library(dplyr)
fruits$rnk <- dense_rank(fruits$dist)
fruits


#220
fruits[fruits$rnk <=3 ,"음식종류"]

#221 최빈값

class1 <- fruits[fruits$rnk <=3 ,"음식종류"]
table(class1)
names(table(class1))[table(class1)==max(table(class1))] # 단백질

setwd("d:\\data")
wbcd <- read.csv("wisc_bc_data.csv",header=T,stringsAsFactors=F)
summary(wbcd)


#이상치 확인

library(outliers)

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}
colnames(wbcd)[-1][-1][2]
head(grubbs.flag(wbcd$radius_mean))
x1 <- grubbs.flag(wbcd$radius_mean)
head(x1)
x2 <- x1[x1$Outlier == TRUE , "Outlier"]
length(x2)
table(x2)
# 결측치 한번에 확인
colSums(is.na(wbcd))


# 각각 이상치
tmp <- colnames(wbcd)[-1][-1]

for ( i in 1:30){
  tmp1 <- grubbs.flag(wbcd[,tmp[i]])
  tmp2 <- tmp1[tmp1$Outlier == TRUE, "Outlier"]
  print(paste(tmp[i],'->',length(tmp2)))
  #print(length(tmp2))
}

wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))
round(prop.table( table(wbcd$diagnosis)) * 100,digit=1)


head(sample(45))

wbcd[sample(10),] # 1~10번까지의 데이터가 섞여서 출력

wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]
nrow(wbcd)

head(wbcd_shuffle[-1])


wbcd2 <- wbcd_shuffle[-1]
str(wbcd2)


normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x)) )
}
wbcd_n <- as.data.frame(lapply(wbcd2[2:31],normalize))
summary(wbcd_n)


train_num <- round(0.9*nrow(wbcd_n),0)
train_num

wbcd_train <- wbcd_n[1:train_num,]
wbcd_test <- wbcd_n[(train_num+1):nrow(wbcd_n),]
nrow(wbcd_test)

wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_train_label
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

install.packages("class")
library(class)

result1 <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_label, k=27)

result1

x <- data.frame('실제' = wbcd_test_label, '예측' = result1)
x
table(x)
library(gmodels)

g2 <- CrossTable(wbcd_test_label,result1,prop.chisq = F)
g2$prop.tbl[4]
print(g2$prop.tbl[1]+g2$prop.tbl[4])


#226
temp<-c()
for ( i in 1:200 ) {
  if  ( i%%2 != 0  ) { 
    wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                          cl = wbcd_train_label,  k=i )
    g2 <- CrossTable(x=wbcd_test_label, y=wbcd_test_pred, chisq=FALSE)
    g3 <- g2$prop.tbl[1] + g2$prop.tbl[4]
    print(g3)
    temp<-append(temp, g3 )
  }
}
temp
plot(temp, type='l', col='red')

knn_fun<-function(){
  
  data1 <- read.csv("d:\\data\\wine.csv", stringsAsFactors=FALSE)
  y <- "Type"
  k_num <-21
  
  normalize<-function(x) {
    return( (x-min(x))/ ( max(x)-min(x)))
  }
  
  #data1 <- data1[-1]
  ncol1 <- which(colnames(data1)==y)
  data1[,ncol1] <- factor(data1[,ncol1], level =c('t1','t2','t3'))
#  str(data1)
  data1_s <- data1[sample(nrow(data1)),]
  data1_n <- as.data.frame(lapply(data1_s[,-ncol1], normalize) )
 # summary(data1_n)
  mm<-round(nrow(data1_n)*(0.9))
  
  data1_train <- data1_n[1:mm, ]
  data1_test <- data1_n[(mm+1):nrow(data1_n), ]
  
  data1_train_label <- data1_s[1:mm,y]
  
  data1_test_label <- data1_s[(mm+1):nrow(data1_n),y]
  
  library(class)
  result1 <- knn(train=data1_train, test=data1_test, cl= data1_train_label, k = k_num )
  
  prop.table( table(ifelse(data1_s[(mm+1):nrow(data1_n),y]==result1,"o","x" )))
  
  # 윈도우 탐색기로 테스트환자를 불러옵니다.
  #fname <- file.choose()
  #table <- read.csv(fname, header=T, stringsAsFactor=F )
  
  # 환자번호를 제외합니다.
  #table <- table[-1]
  
  #라벨 컬럼을 지정합니다.
  #ncol1 <- which(colnames(table)==y)
  
  # 데이터 정규화를 위해 기존 데이터와 묶어줍니다.
  #data1 <- rbind(data1,table)
  #data1_n <- as.data.frame(lapply(data1[,-ncol1], normalize) )
  
  #맨 마지막 환자를 선택합니다.
  #data2_test <- data1_n[nrow(data1_n),]
  
  #result2 <- knn(train=data1_train, test=data2_test, cl= data1_train_label, k = k_num )
  #result2
  
}

knn_fun()



x <- data.frame('실제' = data1_test_label, '예측' = result1)
table(x)
library(gmodels)
CrossTable(data1_test_label,result1,prop.chisq = F)

-------------------------------------
  


rm(data1)
rm(data1_n)
rm(data1_s)
rm(data1_test)
rm(data1_train)
rm(data1_test_label)
rm(data1_train_label)

data1
result1
sum(table(data1_test_label))
sum(table(data1_train_label))
table(data1$Type)[]
data1_s
data1_n

colSums(is.na(data1))


temp<-c()

for ( i in 1:112 ) {
  
  if ( i%%2 != 0 ) {
    
    data1_test_pred <- knn(train=data1_train, test=data1_test, cl= data1_train_label, k = i )
    #data1_train
    #data1_test
    #data1_test_label
    #data1_test_pred
    g2 <- CrossTable(x=data1_test_label, y=data1_test_pred, chisq=FALSE)
    
    g3 <- g2$prop.tbl[1] + g2$prop.tbl[5]+ g2$prop.tbl[9]
    
    temp<-append(temp, g3 )
  }
  
}

temp
plot(temp, type='l', col='red')


setwd("d:\\data")
mushroom <- read.csv("mushrooms.csv", header=T, stringsAsFactors=TRUE)

View(mushroom)
colSums(is.na(mushroom))
# 이상치 확인

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
colnames(mushroom)
tmp <- colnames(mushroom)[-1]

for ( i in 1:30){
  tmp1 <- grubbs.flag(mushroom[,tmp[i]])
  tmp2 <- tmp1[tmp1$Outlier == TRUE, "Outlier"]
  print(paste(tmp[i],length(tmp2)))
  #print(length(tmp2))
}
head(mushroom)
str(mushroom)

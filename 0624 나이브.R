mushroom <- read.csv("mushrooms.csv", header=T, stringsAsFactors=TRUE)

View(mushroom)

mush_test <- mushroom[8123, ]
mush_test

getwd()
write.csv( mush_test, "mush_test.csv",row.names=FALSE )

nrow(mushroom)
mushrooms <- mushroom[ -8123, ]
nrow(mushrooms)

set.seed(1)
dim(mushrooms)

train_cnt <- round( 0.75*dim(mushrooms)[1] )
train_cnt

train_index <- sample( 1:dim(mushrooms)[1], train_cnt, replace=F)
train_index
mushrooms_train <- mushrooms[ train_index, ]
mushrooms_test <- mushrooms[- train_index, ]

nrow(mushrooms_train) # 6092
nrow(mushrooms_test)  # 2031

str(mushrooms_train)


install.packages("e1071")
library(e1071)
model1 <- naiveBayes(type~. , data=mushrooms_train)
model1

result1 <- predict( model1, mushrooms_test[ , -1] )

result1


library(gmodels)

CrossTable( mushrooms_test[ ,1], result1)



model2 <- naiveBayes(type~ . , data=mushrooms_train, laplace=0.0004)

result2 <- predict( model2, mushrooms_test[ , -1] )

CrossTable( mushrooms_test[ ,1], result2)

result3 <- predict( model2, mush_test )

result3
mush_test[1]

temp<-c()
i <- 0.001
while ( i > 0) {
  model2 <- naiveBayes(type~ . , data=mushrooms_train, laplace=i)
  #mush_pred <- knn(train=wbcd_train, test=wbcd_test,cl = wbcd_train_labels, k=i )
  #g2 <- CrossTable(x=wbcd_test_labels, y=mush_pred, chisq=FALSE)
  
  result2 <- predict( model2, mushrooms_test[ , -1] )
  g2 <- CrossTable( mushrooms_test[ ,1], result2)
  g3 <- g2$prop.tbl[1] + g2$prop.tbl[4]
  temp<-append(temp, g3 )
  i = i-0.0001
}
temp
plot(temp, type='l', col='red')

i = 0.001
while (i < 0.1){
  print(i)
  i = i+ 0.001
}


seq(0.001, 0.01 , 0.001)

setwd("d:\\data")
movie <- read.csv("movie.csv", header = T, stringsAsFactors = T)
head(movie)
View(movie)

nrow(movie)

library(e1071)
str(movie)
table(movie)
colnames(movie) <- c("age","gender","job","marry","friend","m_type")

train <-  movie[1:38,]
test <- movie[39,]

test #  장르 로맨틱/ 모델에게는 정답을 알려주지 않고 다른데이터만 넣어서 맞추는지 알아봄

ncol(test)#6

test[-6]
test[6]

model <- naiveBayes(train[,1:5], train[,6], laplace = 0)
model

result <- predict(model, test[-6])
result

test2 <- data.frame(age='20대', gender='여', job='IT', marry='NO',friend='NO')
result <- predict( model, test2)
result 

test3 <- data.frame(age='20대', gender='남', job='학생', marry='NO',friend='NO')
result <- predict( model, test3)
result 
#-------------------------------------------------------------
  
x1 <- menu( c('산포도 그래프','히스토그램 그래프') ,
            
            title='숫자를 선택하세요 ~' )  

x1  
my_func <- function() {
  x1 <- menu( c('산포도 그래프','히스토그램 그래프') ,
              title='숫자를 선택하세요 ~' )  
  switch ( x1,  
           san1 = {   print('산포도 그래프 ~~')  } ,
           san2 = {   print('히스토그램 ~~ '  )  }
  )
}
my_func() 


source('my_func2.R')

r <- function() {source('my_func2.R')}
r()

skin <- read.csv('skin.csv',stringsAsFactors = T)
skin
str(skin)

install.packages("FSelector")
library(FSelector)

weights1 <- information.gain(cupon_react~., skin, unit='log')
print(weights1)


fat <- read.csv("fatliver2.csv", stringsAsFactors = T)
str(fat)
weights2 <- information.gain(FATLIVER~., fat, unit='log')
print(weights2)


install.packages("C50")
library(C50)

skin <- read.csv("skine.csv", header=T ,stringsAsFactors = T)
nrow(skin)
str(skin)
skin_real_test_cust <- skin[30, ]

skin2 <- skin[ 1:29, ]

nrow(skin2)

skin_real_test_cust

skin2 <- skin2[-1]
set.seed(20)
skin2_shuffle <-  skin2[sample(nrow(skin2)),]
train_num <- round(0.7 * nrow(skin2_shuffle), 0)

skin2_train <- skin2_shuffle[1:train_num, ]

skin2_test <- skin2_shuffle[(train_num+1) : nrow(skin2_shuffle), ]

nrow(skin2_train) # 20
nrow(skin2_test) # 9


skin2_test

skin2_train[6]
str(skin2_train)
skin_model <- C5.0(skin2_train[ , -6], skin2_train[,6] , trials = 10,replace= T)
skin_model
skin2_result <- predict( skin_model , skin2_test[ , -6])
skin2_result

CrossTable( skin2_test[ , 6], skin2_result )


credit <- read.csv("credit.csv",header = T,stringsAsFactors = T)
str(credit)
head(credit)
prop.table( table(credit$default) )
summary( credit$amount)
set.seed(123)
credit_shuffle <- credit[ sample(nrow(credit)),]
train_num <- round(0.9*nrow(credit_shuffle),0)
credit_train <- credit_shuffle[1:train_num,]
credit_test <- credit_shuffle[train_num+1:nrow(credit_shuffle),]
#ncol(credit)
str(credit_train)
credit_model <- C5.0(credit_train[,-17],credit_train[,17],trials = 20)
credit_model
credit_result <- predict(credit_model,credit_test[,-17])
library(gmodels)
x<-CrossTable( credit_test[ , 17], credit_result )
x



install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')


credit = read.csv('d:/data/credit.csv',header=T,stringsAsFactors = T)



# 데이터 형태 확인

str(credit) # 수치형, 명목형 섞여있음



# 데이터 분류 (caret 사용)

library(caret)

set.seed(5)

intrain = createDataPartition(credit$default,p=0.9,list=F)



# train(90%) / test(10%)

credit_train = credit[intrain,]

credit_test = credit[-intrain,]



nrow(credit_train) # 900

nrow(credit_test) # 100



# 의사결정트리 모델 생성

library(C50)

credit_model = C5.0(default~.,data=credit_train,trials=24) # 24 : 0.87

credit_result = predict(credit_model,credit_test[,-17])



# 이원 교차표로 결과 확인

library(gmodels)

x = CrossTable(credit_test[,17],credit_result)



library(rpart)

library(rpart.plot)



rpartmod = rpart(default~., data=credit_train, method='class')

rpart.plot(rpartmod)



x$prop.tbl[1]+x$prop.tbl[4] # 0.87

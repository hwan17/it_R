setwd("d:\\data\\csv\\")
boston<-read.csv("boston.csv", stringsAsFactors = T)
str(boston)
head(boston)
# 정규화 함수

normalize <- function(x) {  
  return((x - min(x)) / (max(x) - min(x)))
}


# 전체 데이터 프레임에 정규화 적용

boston_norm <- as.data.frame(lapply(boston, normalize))


# 0과1 사이에 범위 확인

summary(boston_norm$MEDV)


# 본래 데이터의 최소값, 최대값 비교

summary(boston$MEDV)
#ㄴ 집값이 최소 5,000달러 ~ 최대 50,000달러로 집값이 분포되어 있다.


# 히스토그램 그래프 확인
hist(boston$MEDV)
#ㄴ 정규분포는 평균에 가까운 중간정도의 가격이 많고 가격이 아주 작거나 큰 데이터는 적은 종모양의 분포
#ㄴ 평균에 가까운 집값의 분포는 10,000달러 ~ 25,000달러로 구성되어 있다.

# 훈련과 테스트 데이터 생성

dim(boston_norm)
#ㄴ 몇행 몇열인지 확인하는 함수


# 훈련데이터와 테스트 데이터를 7:3으로 나눈다.

set.seed(1)

s_cnt<-round(0.7*(nrow(boston_norm)))

s_index<-sample(1:nrow(boston_norm), s_cnt, replace=F)

boston_train <- boston_norm[s_index, ]

boston_test <- boston_norm[-s_index, ]

head(boston_train)



## 3단계 : 데이터로 모델 훈련 ----

# neuralnet 모델 훈련
library(neuralnet)

# 하나의 은닉 뉴런에 대한 단순한 ANN
boston_model <- neuralnet(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, data=boston_train, hidden=10)
#ㄴ설명 : 입력층 --> 은닉1층 --> 출력층
#          14개       10개       1(집값)
#           0층        1층          2층
#   2층 신경망


## 4단계 : 모델 성능 평가 ----
# 모델 결과

model_results <- compute(boston_model, boston_test[1:13])
#ㄴ설명 : 테스트 데이터를 신경망 모델에 넣어서 테스트 데이터의 집값을 예측


# 예측한 집값을 선택
predicted_houseprice <- model_results$net.result

# 예측값과 실제값간의 상관 관계 확인
cor(predicted_houseprice, boston_test$MEDV)

plot(boston_model)


prop.table( table(boston) )



--------------------------------------
  
install.packages("arules")
library(arules)
x <- data.frame(
  beer=c(0,1,1,1,0),
  bread=c(1,1,0,1,1),
  cola=c(0,0,1,0,1),
  diapers=c(0,1,1,1,1),
  eggs=c(0,1,0,0,0),
  milk=c(1,0,1,1,1) )

trans <- as.matrix( x, "Transaction")
trans


rules1 <- apriori(trans, parameter=list(supp=0.2, conf=0.6,target="rules") )

rules1

inspect(sort(rules1))




install.packages("sna")
install.packages("rgl")
library(sna)
library(rgl)

b2 <- t(as.matrix(trans)) %*% as.matrix(trans)
b2
library(sna)
library(rgl)
b2.w <- b2 - diag(diag(b2))
b2.w
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "green" , edge.col="blue" , boxed.labels=F ,arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2)

#--------------------
  
  
  
build <- read.csv("d:\\data\\csv\\building.csv" , header = T)
build[is.na(build)] <- 0
build <- build[-1]
build
trans <- as.matrix(build , "Transaction")
rules1 <- apriori(trans , parameter = list(supp=0.2 , conf = 0.6 ,target = "rules"))
rules1
inspect(sort(rules1))


rules2 <- subset(rules1 , subset = lhs %pin% '보습학원' & confidence > 0.7)
inspect(sort(rules2))

rules3 <- subset(rules1 , subset = rhs %pin% '편의점' & confidence > 0.7)
rules3
inspect(sort(rules3))


#visualization
b2 <- t(as.matrix(build)) %*% as.matrix(build)
b2.w <- b2 - diag(diag(b2))
rownames(b2.w)
colnames(b2.w)
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "green" , edge.col="blue" , boxed.labels=F , arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2)



#-------------------------------



paper <- read.csv("11_meal_m.csv" , header = T)
paper[is.na(paper)] <- 0
paper
rownames(paper) <- paper[,1]
paper <- paper[-1]
paper2 <- as.matrix(paper)
paper2

View(paper2)
book <- read.csv("book_hour.csv" , header = T)
paper2
book

library(sna)
x11()
gplot(paper2 , displaylabels = T, boxed.labels = F , vertex.cex = sqrt(book[,2]) , vertex.col = "blue" , vertex.sides = 20 ,
      edge.lwd = paper2*2 , edge.col = "green" , label.pos = 3)

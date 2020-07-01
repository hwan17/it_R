#분류를 위한 nnet 신경망 패키지 설치
install.packages("nnet")
library(nnet)

#이원 교차표를 보기 위해 gmodels 라이브러리
library(gmodels)

#와인 데이터를 R로 로드
wine <- read.csv("d:\\data\\csv\\wine.csv",stringsAsFactors = T)
head(wine) #Type이 라벨(정답)이고 t1,t2,t3 로 3가지 등급이 있다.
str(wine)
wine.scale <- cbind(wine[1], scale(wine[-1]))
size <- nrow(wine.scale)
set.seed(100)
index <- c(sample(1:size, size * 0.7))
train <- wine.scale[index, ]
test <- wine.scale[-index, ]
model.nnet2 <- nnet(Type ~ ., data = train, size = 2, decay = 5e-04, maxit = 200)

predicted <- predict(model.nnet2, test, type = "class")
predicted
actual <- test$Type
model.confusion.matrix <- table(actual, predicted)

CrossTable(model.confusion.matrix)
----------------
# scale함수로 정규화 ( 평균이 0, 표준편차 1 )
wine.scale <- cbind(wine[1],scale(wine[-1]))
summary(wine.scale)

# 와인데이터를 shuffle하고 훈련데이터와 테스트데이터로 나눈다.
size <- nrow(wine.scale)
set.seed(100)
index <- c(sample(1,size, size*0.7))
train <- wine.scale[index, ]
test <- wine.scale[-index, ]

# 와인데이터의 등급을 분류하는 신경망의 모델 생성
#model.nnet2 <- nnet(Type~., data = train, size=2, decay= 5e-04, maxit = 200)
model.nnet2 <- nnet(Type ~ ., data = train, size = 2, decay = 5e-04, maxit = 200)


# size : number of unuit in the hidden layer.
# decay : parameter for weight decay . Default 0.
#   가중치 감소라고 해서 신경망이 학습하다 보니까 고양이 귀에 대한 가중치를 높게 부여
#   이 신경망에 귀가 없는 고양이 사진을 넣었더니 개라고 함
#   지나치게 크게 부여된 가중치를 감소시켜주는 파라미터
# maxit : maximum number of iterations. Default 100.
#    178개의 data를 100번 반복
predicted <- predict(model.nnet2, test, type = "class")
predicted
actual <- test$Type
model.confusion.matrix <- table(actual, predicted)

CrossTable(model.confusion.matrix)


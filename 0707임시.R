

## Estimating Future Performance ----
# partitioning data

setwd("d:\\data\\csv")
#install.packages("caret") 

library(caret)
credit <- read.csv("credite.csv",stringsAsFactors = T) # 독일 은행의 채무 불이행자를 예측
nrow(credit) #1000
ncol(credit) #17
# 하기 위한 데이터 

# Holdout method
# using random IDs

random_ids <- order(runif(1000)) # 난수 1000개 생성 
random_ids
credit_train <- credit[random_ids[1:500],]  # 훈련 50%
credit_validate <- credit[random_ids[501:750], ] # 검정 25%
credit_test <- credit[random_ids[751:1000], ]  # 테스트 25%
nrow(credit_train)

table(credit$default)

#클래스가 2개(채무 이행자/채무 불이행자)라면 
#(train/test/validate)내 클래스 비율에 차이가 나는 문제가 생길 수 있다.
# 훈련데이터 70:30
# 테스트데이터 70:30

# 이것을 해결하는 방법이 바로 층별 랜덤 샘플링
# 관련함수 createDataPartition 함수

# using caret function

in_train <- createDataPartition(credit$default, p = 0.8, list = FALSE)
nrow(in_train)
credit_train <- credit[in_train, ] # 훈련 데이터 구성
credit_test <- credit[-in_train, ] # 테스트 데이터 구성 
str(credit_train)
nrow(credit_test)
# 10-fold CV

folds <- createFolds(credit_train$default, k = 10)
str(folds)
#설명 : 전체 10fold로 교차검증 수행하기 위해 샘플링 된 인덱스가 생성됨

credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]
nrow(credit01_test)
nrow(credit01_train)
#전체 10폴드 교차검증을 수행하려면 이 단계는 10회 반복되어야한다. 

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
install.packages("irr")

library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv",stringsAsFactors = T)


set.seed(123)

random_ids <- order(runif(1000)) # 난수 1000개 생성 
creditn <- credit[random_ids,]
in_train <- createDataPartition(creditn$default, p = 0.8, list = FALSE)

credit_train1 <- creditn[in_train, ] # 훈련 데이터 구성
credit_test1 <- creditn[-in_train, ] # 테스트 데이터 구성 
#str(credit_test1[-17])
folds <- createFolds(credit_train1$default, k = 10)
folds$
cv_results <- lapply(folds, function(x) {
  credit_train <- credit_train1[-x, ]#
  credit_test <- credit_train1[x, ]#
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test[-17])
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)

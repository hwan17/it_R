# 문제 265. 
setwd("d:\\data")
insu <- read.csv("csv\\insurance.csv",stringsAsFactors = T)
head(insu)
attach(insu)
lm(expenses~age+children+smoker+bmi+region,insu)
fage = readline('나이')
fchildren = readline('부양가족수')
fsmocker = readline('흡연여부 (yes/no)')
fbmi = readline('bmi (16~53)')
fregion = readline('거주지역 (southwest/southeast/northwest/northeast)')
if (fregion == 'northeast'){
  fregion2 = 0
} else if (fregion == 'northwest'){
  fregion2 = -352
} else if (fregion == 'southeast'){
  fregion2 = -1034.9
} else if (fregion == 'southwest'){
  fregion2 = -958.6
}
re = -19993.3 + 257*as.integer(fage) + 474.8*as.integer(fchildren) + 23835.2*ifelse(fsmocker=='yes',1,0) + 338.8*as.double(fbmi) + fregion2
re


#--------------------
ww <- read.csv("csv\\whitewines.csv")
ww
View(ww)

unique(ww$quality)


#answp266

tee <- c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)

# a속성으로 나누었을 때
at1 <- c(1,1,1,2,2,3,4,5,5)
at2 <- c(6,6,7,7,7,7)

#b속성으로 나누었을 때
bt1 <- c(1,1,1,2,2,3,4)
bt2 <- c(5,5,6,6,7,7,7,7)

# a속성으로 나누었을 때의 SDR
sd(tee)
sd(at1)
sd(at2)
sdr_a = sd(tee) - (length(at1)/length(tee)*sd(at1) + length(at2)/length(tee)*sd(at2) )
sdr_a
sdr_b = sd(tee) - (length(bt1)/length(tee)*sd(bt1) + length(bt2)/length(tee)*sd(bt2) )
sdr_b

# 둘중에 SDR이 높은 것으로 분류

# b속성으로 분류한 원본 데이터의 두영역의 평균값을 각각 구해서 등급을 예측
mean(bt1)
mean(bt2)
----------

wine <- read.csv("d:\\data\\csv\\whitewines.csv")
#2. 와인의 quality 데이터가 정규분포에 속하는 안정적인데이터 인지 확인

hist(ww$quality)

#3. wine 데이터를 train 데이터와 test 데이터로 나눈다.

wine_train <- ww[1:3750, ]
wine_test <- ww[3751:4898, ]

#4. train 데이터를 가지고 model 을 생성한다.

library(rpart)

model <- rpart( quality ~ . , data=wine_train)

model

library(rpart.plot)
rpart.plot( model, digits=3)

rpart.plot(model, digits=3, fallen.leaves=T, type=3, extra=101)

result <- predict(model, wine_test)

cor(result, wine_test$quality)


MAE <- function( actual, predicted) {
  mean( abs( actual - predicted) )
}

MAE( result, wine_test$quality)

#--------------
#모델트리

install.packages("Cubist")
library(Cubist)



wine <- read.csv("d:\\data\\csv\\whitewines.csv")
#2. 와인의 quality 데이터가 정규분포에 속하는 안정적인데이터 인지 확인

hist(ww$quality)

#3. wine 데이터를 train 데이터와 test 데이터로 나눈다.

wine_train <- ww[1:3750, ]
wine_test <- ww[3751:4898, ]

#3. 와인의 품질을 예측하는 모델을 생성한다.

#model_tree <- M5P(quality ~ ., data=wine_train)
#model_tree
m <- cubist(wine_train[-12],wine_train$quality)
m
#4. 만든 모델과 테스트 데이터로 예측을 한다.

#p.m5p <- predict(model_tree, wine_test)
p <- predict(m, wine_test)
p
#5. 예측값(p.m5p) 과 테스트 데이터의 라벨간의 상관관계를 확인한다

cor( p , wine_test$quality )


#6. 예측값(p.m5p) 과 테스트 데이터의 라벨간의 평균절대오차를 확인한다.

MAE( wine_test$quality, p)


#------------


install.packages("rmarkdown")
install.packages("knitr")

library(rmarkdown)
library(knitr)

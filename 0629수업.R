# 문제255. 전치행렬 구하기
a <- matrix(c(1,2,3,4,5,6),3,2,byrow=T)
a
t(a)

# 문제256. 아래 행렬 곱 구현
b <- matrix(c(1,2,3,2,3,4), nrow = 2, ncol = 3, byrow = T)
b
c <- diag(3)
c
b%*%c

# 
d <- matrix(c(1,2,3,4), nrow = 2,byrow=T)
d
d <- cbind(intercept =1 ,d)
e <- solve(d)
round(d %*% e)


# 문제258.

reg <- function(y,x){
  x <- as.matrix(x) 
  x <- cbind(intercept=1, x) 
  b <-  solve(t(x)%*% x) %*% t(x) %*% y
  colnames(b) <-"estimate"
  print(b)
}

#259
setwd("d:\\data")
launch <- read.csv("csv\\challenger.csv")
head(launch)
x = launch[-1]
x
reg(y=launch$distress_ct,x=x)
attach(launch)
lm(distress_ct~temperature,launch)
reg



#260
ce <- read.csv("csv\\multi_hg.csv")
head(ce)
ce_t <- ce[-4]
reg(ce$만족감,ce_t)
re


#261

attach(ce)
lm(만족감~외관+편의성+유용성,ce)
lm(만족감~.,ce)



#262
sp <- read.csv("csv\\sports.csv")
head(sp)

attach(sp)
lm(acceptance~.,sp[-1])


#263. 정규화를 하고 회귀계수(기울기) 확인

#1. 데이터 로드

sp <- read.csv("csv\\sports.csv")
head(sp)

#2. 학생번호 제외
sp <- sp[-1]
sp


#3. 정규화 작업 수행

normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
sp_n <- as.data.frame(lapply(sp, normalize))
sp_n

#4. 회귀분석
attach(sp_n)
lm(acceptance~.,sp_n)



insu <- read.csv("csv\\insurance.csv", stringsAsFactors = T)
head(insu)
str(insu)
colSums(is.na(insu))


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

grubbs.flag(insu[,"bmi"])


hist(insu$expenses)


cor( insu[ , c("age","bmi","children","expenses")] )

install.packages("psych")
library(psych)
pairs.panels( insu[c('age','bmi','children','expenses')])


attach(insu)

lm(expenses ~ ., data= insu)


# 263



mo <- lm(expenses ~ ., data= insu)
summary(mo)


#5단계 성능개선

insu$age2 <-insu$age^2
head(insu)
lm(expenses ~ ., data= insu)
mo <- lm(expenses ~ ., data= insu)
summary(mo)

insu$bmi30 <-  ifelse(insu$bmi>=30,1,0)
lm(expenses ~ ., data= insu)
mo <- lm(expenses ~ ., data= insu)
summary(mo)




###3
mo <- lm(expenses ~ age+children+bmi+sex+bmi30*smoker+region*age2, data= insu)
summary(mo)


mo <- lm(expenses ~ age+age2+children+bmi+sex*region+bmi30*smoker*region+region*age, data= insu)
summary(mo)


insu$south <- ifelse(insu$region=="southwest" | insu$region=="southeast",1,0)
mo <- lm(expenses ~ age+age2+children+bmi+sex+bmi30*smoker*south+south, data= insu)
summary(mo)


test <- read.csv("csv\\test_vif2.csv")
test
# 상관관계 확인(아이큐-공부시간)

cor( test[, c("아이큐","공부시간")])
cor(test[-1])
test <- test[-1]

m1 <- lm(test$시험점수~아이큐+공부시간+등급평균, test)
#summary(m1)
vif(m1)

m2 <- lm(test$시험점수~공부시간+등급평균, test)
#summary(m2)
vif(m2)


m1 <- lm(test$시험점수~아이큐+공부시간, test)
summary(m1)
#library(car)
vif(m1)

##############
library(car)
test <- read.csv("csv\\test_vif2.csv")

test <- test[-1]


cor(test)


m1 <- lm(test$시험점수~아이큐+공부시간+등급평균, test)
summary(m1)
vif(m1)

m2 <- lm(test$시험점수~공부시간+등급평균, test)
summary(m2)
vif(m2)



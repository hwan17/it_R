usedcars <- read.csv("d:\\data\\usedcars.csv")
usedcars
usedcars$price


#--------------------
  
  
  
# 182
class1 <- c( rep(19,3), rep(20,7), rep(21,3), 145, 147 )
median(class1)
summary(class1)


summary(usedcars$price)
summary(usedcars$mileage)

mean(usedcars$price)
median(usedcars$price)

summary(usedcars)

x2<-boxplot(usedcars$mileage)
x2$out

library(plotly)

prc <- rnorm(150,13000,3122)
sd(usedcars$price)
sum(table(usedcars$price))
p1 <- plot_ly(x = ~prc, type = "histogram")
p1
prc2 <- sort(usedcars$price)
p2 <- plot_ly(x = ~prc2, type= "histogram")
p2

par(mfrow=c(1,2))
hist(usedcars$price)
hist(usedcars$mileage)

install.packages("fBasics")
library(fBasics)
skewness(usedcars$mileage)

class2 <- sort(usedcars$mileage)
class2
hist(class2)
par(new =T)
plot(class2, dnorm(class2, mean = mean(class2), sd=sd(class2)), type='l', main = "주행거리 정규분포 그래프")


t <- sort(iris$Petal.Length)
plot(t, dnorm(t,mean = mean(t), sd=sd(t)),type='l')




library("plotly")

car_price <- plot_ly(x = ~usedcars$price,
                     
                     type = "histogram") %>% 
  
  layout(title = "중고차 가격 히스토그램",
         
         xaxis = list(title = "price",
                      
                      zeroline = FALSE),
         
         
         
         
         yaxis = list(title = "Count",
                      
                      zeroline = FALSE))

car_price


plot(emp$comm,emp$sal, pch=21, col='orchid',bg='orchid')

cor(emp$comm,emp$sal)

emp$comm[is.na(emp$comm)] <- 0
cor(emp$comm,emp$sal)


plot(usedcars$mileage, usedcars$price,pch=21, col='orchid', bg='orchid')
cor(usedcars$mileage,usedcars$price)



install.packages("gmodels")
library(gmodels)

attach(emp)
tapply(empno, list(deptno, job), length, default = 0)


CrossTable(x=emp$deptno,y=emp$job)


library(data.table)

data.table(emp$sal, emp$sal>=2500)
emp$sal_tf <- emp$sal>=2500
emp
library(gmodels)
CrossTable(emp$job, emp$sal_tf)


usedcars$conservative <- usedcars$color %in% c("Black","Gray","Silver","White")

table(usedcars$conservative)
CrossTable(x= usedcars$model, y=usedcars$conservative,chisq = T)
CrossTable(x=usedcars$conservative , y=usedcars$model,chisq = T)


1 - pchisq(q=8.33, df=1, lower.tail = T)


income <- function(name) {
  sal <- emp[ emp$ename==toupper(name), "sal"]
  print(sal)
}

income('scott')

income2 <- function(name) {
  sal <- emp[ emp$job==toupper(name), "sal"]
  print(sal)
}

income2('SALESMAN')


income2 <- function() {
  name <- readline(prompt = "직업 입력 :")
  sal <- sum(emp[ emp$job==toupper(name), "sal"])
  return (paste("토탈월급은",sal,"입니다"))
}

income2()



triangle <- function(){
  l1 <- as.integer(readline(prompt = "밑변 길이 입력 :"))
  l2 <- as.integer(readline(prompt = "높이 입력 :"))
  l3 <- as.integer(readline(prompt = "빗변 길이 입력 :"))
  
  if (l1^2 + l2^2 == l3^2) {
    return ("직각 삼각형이 맞습니다.")
  }
  else {
    return ("직각 삼각형이 아닙니다.")
  }
}

triangle()



aaa <- function(x) {
  for ( i in 1:x ) {
    
    print (i)
    
  }
}

aaa(10)


star <- function(x) {
  for ( i in 1:x){
    print(rep('★',i))
  }
}
star(5)

runif(1)^2
cnt <- 0
cnt <- cnt +1
cnt
a <- c(runif(1),runif(1))
a[1]

mon <- function(n) {
  cnt <- 0
  for ( i in 1:n){
    if (runif(1)^2 + runif(1)^2 <= 1){
      cnt = cnt +1
    }
  }
  return (cnt/n*4)
}

mon(100000)


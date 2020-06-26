# 문제243.

setwd("d:\\data")
mushroom<-read.csv("mushrooms.csv", stringsAsFactors = T)
str(mush)

library(FSelector)

weight1 <- information.gain(type~.,mush,unit = 'log')
weight1

# 문제244. 

library(doBy)
orderBy(~-attr_importance,weight1)

#oneR 실습

set.seed(11)

dim(mushroom)

train_cnt <- round( 0.75 * dim(mushroom)[1])
train_index <- sample(1:dim(mushroom)[1], train_cnt, replace=F)

mushroom_train <- mushroom[train_index, ]
mushroom_test <- mushroom[-train_index, ]



library(OneR)

model1 <- OneR(type~. , data=mushroom_train)

model1
summary(model1)

result1 <- predict( model1, mushroom_test[ , -1] )

library(gmodels)

CrossTable( mushroom_test[ , 1], result1)

#JRip

install.packages("RWeka")
library(RWeka)

model2 <- JRip(type~ ., data=mushroom_train)
model2

summary(model2)



result2 <- predict( model2, mushroom_test[, -1] )

library(gmodels)

CrossTable( mushroom_test[ , 1], result2)

#--------------------------

r <- function() { source('my_func2.R') }
r()


#-------------------------------
set.seed(11)
setwd("d:\\data")
mushroom<-read.csv("mushrooms.csv", stringsAsFactors = T)
dim(mushroom)

train_cnt <- round( 0.75 * dim(mushroom)[1])
train_index <- sample(1:dim(mushroom)[1], train_cnt, replace=F)
head(train_index)
mushroom_train <- mushroom[train_index, ]
#mushroom_test <- mushroom[-train_index, ]

library(RWeka)
fname <- file.choose()  
mushroom_test <- read.csv(fname, header=T, stringsAsFactor=T )    
model2 <- JRip(type~ ., data=mushroom_train)
result2 <- predict( model2, mushroom_test[, -1] )
print(ifelse(result2=='edible',"식용입니다.","식용이아닙니다."))



# 249


x = c(10,20,30,40)
y = c(300,250,200,150)
mx<-mean(x)
my<-mean(y)

sum((x-mx)*(y-my))/(sum((x-mx)^2))
# y = ax + b
a =cov(x,y)/var(x)
b = my - a*mx
x = 35
v = a*x+b
v


f_nu <- function(n){
  x = c(10,20,30,40)
  y = c(300,250,200,150)
  mx<-mean(x)
  my<-mean(y)
  a =cov(x,y)/var(x)
  b = my - a*mx
  v = a*n+b
  print(v)
}

f_nu(35)

# 
setwd("d:\\data")
reg <- read.table("regression.txt", header=T)
head(reg)
plot(reg$tannin,reg$growth)


attach(reg)

plot(growth~tannin, data = reg, pch=21, col='blue', bg='red')

m <- lm( growth ~ tannin, data=reg)
m
abline(m, col='red')

title(paste( '성장률=', round(m$coefficients[2], 4), "* 탄닌 + ", round(m$coefficients[1], 4)))
y_hat <- predict(m, tannin=tannin)
y_hat
join <- function(i){
  lines( c(tannin[i], tannin[i]), c( growth[i],y_hat[i]),col="green")
}
sapply(1:9, join)



# 
setwd("d:\\data")
ad <- read.csv("csv\\simple_hg.csv", header=T)
head(ad)
#plot(reg$tannin,reg$growth)

attach(ad)

plot(input~cost, data = ad, pch=21, col='blue', bg='red')

m <- lm(input~cost, data = ad)
m
abline(m, col='red')

title(expression(italic(paste( '매출=', round(m$coefficients[2], 4), "* 광고비 + ", round(m$coefficients[1], 4)))))
y_hat <- predict(m, cost=cost)
y_hat
join <- function(i){
  lines( c(cost[i], cost[i]), c( input[i],y_hat[i]),col="green")
}
sapply(1:15, join)


launch <- read.csv("csv\\challenger.csv")
launch

attach(launch)
lm( distress_ct ~ temperature, launch )

plot( distress_ct ~ temperature, data=launch,col="red", bg="red", pch=21)
m <- lm( distress_ct ~ temperature, launch)
abline( m , col="blue")

title(paste('파손수=', round(m$coefficients[1], 4), "* 온도 + ",
            round(m$coefficients[2], 4)))

cor(temperature,distress_ct)

#-------------
install.packages("car")
library(car)
data(Boston, package =  "MASS")
Boston


head(Boston)


model <- lm(medv~., data = Boston)
model
vif(model) > 10


#---------------------



k_index <- read.csv("csv\\K_index.csv", header=T,stringsAsFactors=F)

s_stock <- read.csv("csv\\S_stock.csv", header=T,stringsAsFactors=F)

h_stock <- read.csv("csv\\H_stock.csv", header=T,stringsAsFactors=F)

all_data <- merge(merge(k_index,s_stock), h_stock)

head(all_data)

attach(all_data)

plot(k_rate, s_rate, col="blue")

plot(k_rate, h_rate, col="blue")


#_________

plot(k_rate, s_rate, col="blue")

model_s <- lm( s_rate ~ k_rate, data=all_data)

abline( model_s, col="red")



#------------------------------




r <- function() { source('my_func2.R') }
r()

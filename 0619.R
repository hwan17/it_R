a <- c(1,2,3,4,5)
str(a)

b <- seq(1,20)
b

c <- c(rep(1,5),rep(2,6),rep(3,8),rep(4,6))
c

d <- c(rep('jones',4),rep('king',3),'scott')
d


d2 <- table(d      )
d2[-3]
d2[-2:-3]

d
d[-3]
c
b
b[-1]


f1 <- c("middle","low","high")
f1
str(f1)
f2 <- factor(f1)
f2
f3 <- factor(f1,order= T, level=c("low","middle","high"))
f3

min(f3)

order(f3,decreasing = T)
f3[order(f3,decreasing = F)]


blood <- factor( c("O","AB","A"), levels=c("A","B","AB","O"))
blood[1:2]


symptoms <- factor( c("SEVERE","MILD","MODERATE"),
                    levels=c("MILD","MODERATE","SEVERE"),
                    ordered = TRUE)

symptoms

symptoms > "MODERATE"



k1 <- data.frame( x=c(1,2,3,4,5), y=c(2,3,4,5,10) )
k1
str(k1)
k1[5,"y"]

setwd("d:\\data")
emp <- read.csv("emp3.csv" , header=T)
str(emp)


emp <- read.csv("emp3.csv" , header=T,stringsAsFactors = TRUE)
str




ma <- matrix( c(1:9), nrow=3, ncol=3, byrow = T)
mb <- matrix( c(1:9), nrow=3, ncol=3)
ma%*%mb



array( c(1:12) , dim=c(3,4) )
array( c(1:12) , dim=c(2,2,3) ) # 3차원
array( c(1:12) , dim=c(2,2,2,2) ) # 4차원



setwd("d:\\data")
 emp <- read.csv("emp3.csv", header=T)

 
install.packages("xlsx")
library(xlsx)
dept <- read.xlsx("dept.XLS", 1)
dept


niv <- readLines("NIV.txt")
head(niv)


install.packages("DBI")
install.packages("RJDBC")
library("DBI")
library("RJDBC")


driver <- JDBC('oracle.jdbc.driver.OracleDriver', 'ojdbc8.jar')
oracle_db <- dbConnect(driver, 'jdbc:oracle:thin:@//127.0.0.1:1521/orcl', 'scott', 'tiger')

emp_query <- 'select ename, sal, deptno from emp where deptno=20'

emp_data <- dbGetQuery(oracle_db, emp_query)

emp_data



ucar <- read.csv("usedcars.csv",header = T)
ucar



max(ucar$mileage)

summary(ucar)



class1 <- c( rep(19,3), rep(20,6), rep(21,3), 145, 147 )
class1
table(class1)

mean(class1)

install.packages("outliers")
library(outliers)
outlier(class1)
x2 <- boxplot(class1)
x2
x2$out

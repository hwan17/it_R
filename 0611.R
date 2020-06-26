emp


pie(emp$sal, label=emp$ename, col=rainbow(3))


setwd("C:\\data")
getwd()

emp <- read.csv("emp3.csv",header = T)
emp

attach(emp)
tapply(sal, list(deptno,job),sum)

str(emp)

emp[ , c("ename","sal")]

emp[emp$sal>=2000,c("ename","sal")]

x <- c(1,2,3)
x

x> c(1,1,1) & x<c(3,3,3)


x2 <- 1
x2 > -2 && x2 <2
emp[emp$job=='SALESMAN',c('ename','sal','job')]
emp[emp$job=='SALESMAN'& emp$sal>=1000,c('ename','sal','job')]
paste(emp$ename,' 의 직업은',emp$job)

install.packages("data.table")

library(data.table)
data.table(paste(emp$ename,' 의 직업은',emp$job))

emp[!emp$job %in% c('SALESMAN','ANALYST'),c('ename','sal','job')]


emp[!is.na(emp$comm) ,c('ename','sal','comm')]


emp[grep("^..L",emp$ename) ,c('ename','sal')]



a <- c(1,1,2,3,4,5,6,6,6,6,6,6)
a <- unique(a)
unique(emp$deptno)
library(data.table)
data.table("부서번호" = unique(emp$deptno))

b = emp[emp$deptno == 20 , c('ename','sal','deptno')]
b[order(b$sal , decreasing = T),]

ls()
rm(b)

install.packages("doBy")
library(doBy)
orderBy( ~-sal, emp[emp$deptno==20, c("ename","sal","deptno")])

crime_loc <- read.csv("crime_loc.csv",header = T)
head(crime_loc)

x <- crime_loc[ crime_loc$범죄=='살인',]
orderBy(~-건수, x)
unique(crime_loc$범죄)

library(data.table)
data.table("범죄유형" = unique(crime_loc$범죄))

tolower(emp$ename)

cbind(tolower(emp$ename), tolower(emp$job))

data.table(이름 = tolower(emp$ename), 직업 = tolower(emp$job))

emp$ename


substr(emp[substr(emp$ename,2,2) == 'M', c('ename') ],2,4)

gsub(0,'*',emp[,c("ename","sal")])

     
data.table(emp$ename,gsub('[0-2]','*',emp$sal))  
10%%3
    
orderBy( ~-연봉,data.table(이름=emp$ename,연봉=emp$sal*12)  )

orderBy( ~-연봉,data.table(이름=emp$ename,연봉=round(emp$sal*12,-3)) )

orderBy( ~-연봉,data.table(이름=emp$ename,연봉=trunc(emp$sal*12,-3)) )


Sys.Date()

data.table(emp$ename, Sys.Date() - emp$hiredate)

data.table(emp$ename, Sys.Date() - as.Date(emp$hiredate))

str(emp)

install.packages("lubridate")
library(lubridate)
ceiling_date(Sys.Date(), "month")-1
floor_date(Sys.Date(),"month")

last_day <- function(x) {
  
  ceiling_date(x, "month")  - days(1)
  
}

last_day(Sys.Date())


first_day <- function(x) {
  
  floor_date(x, "month")
  
}

first_day <- function(x) {
  
  floor_date(x, "month")
  
}
rm(x)

Sys.Date() + years(100)

data.table(emp$ename, format(as.Date(emp$hiredate),'%A'))


format(as.Date('1994-01-07'),'%A')           
format(1994-01-07,'%A')

data.table(emp$ename, substr(emp$hiredate,1,4))
data.table(emp$ename, format(as.Date(emp$hiredate),'%Y'))

data.table(emp$ename, emp$hiredate)


emp[format(as.Date(emp$hiredate),'%A') == '수요일', c('ename','hiredate')]




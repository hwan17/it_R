emp <- read.csv("d:\\data\\emp3.csv")
emp

a <- c()
for ( i in 1:length(emp[emp$job=="SALESMAN","sal"])) {
  a[i] <- emp[emp$job=="SALESMAN","sal"][i]
}
a

length(emp[emp$job=="SALESMAN","sal"])

a <- c()
for ( i in 1:length(emp[emp$job=="SALESMAN","sal"])) {
  a[i] <- emp[emp$job=="SALESMAN","sal"][i]
}
a


a<-c()

for (i in 1:length(emp$sal)) {
  if (emp$job[i]=='SALESMAN'){
    a<-a+emp$sal[i]
    
  }
}

a


b<-3
b<-b+3

b<-0

for (i in 1:length(emp$sal)) {
  if (emp$deptno[i]==20){
    b<-b+emp$sal[i]
    
  }
}

b


a <- c()
for ( i in 1:length(emp[emp$deptno==20,"sal"])) {
  a[i] <- emp[emp$deptno==20,"sal"][i]
}
a
sum(a)

emp[emp$deptno==20,"sal"]


c <- function() {
  for(0) {
    if ( {
      
    })
  }
}

income <- function(n) {
  b<-0
  for (i in 1:length(emp$sal)) {
    if (emp$deptno[i]==n){
      b<-b+emp$sal[i]
    }
  }
  return (b)
}

income(30)
eval(emp$"sal")
v <- "a"
w <- "b"
paste(v,w)

x <- c()

for ( i in 1:length(emp$sal) ) {
  
  x[i] <- emp$sal[i]
  
}
cat('최대값', max(x), '\n',
    '최소값', min(x), '\n',
    '평균값', mean(x),'\n',
    '중앙값', median(x),'\n',
    '분산값', var(x) , '\n',
    '표준편차',sd(x) )
cat('a',max(x),',','b',min(x))


stats <- function(){
  name1 <- readline(prompt = "테이블 명 :")
  name2 <- readline(prompt = "컬럼 명 :")
  x <- c()
  for ( i in 1:length(eval(name1,'$',name2) ) {
    x[i] <- eval(name1,'$',name2,'[',i,']')
  }
  return ( cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) ) )
}

stats()


eval('emp''$sal')

stats <- function() {
  
  t_name <- readline('테이블명 입력')
  table_name = get(t_name)
  xcol_name <- readline('컬럼명 입력')
  xdata <- table_name[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) )
}

stats()



fname <- file.choose()
fname

table <- read.csv(fname, header =T)
head(table)
----------------------------------------------


stats <- function() {
  library(data.table)
  fname <- file.choose()
  table <- read.csv(fname, header =T)
  print( data.table(컬럼명= colnames(table)))
  xcol_name <- readline('컬럼명 입력')
  xdata <- table[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) )
}

stats()

--------------------------
  
stats <- function() {
  fname <- file.choose()
  fname
  
  table <- read.csv(fname, header =T)
  
  #t_name <- readline('테이블명 입력')
  #table_name = get(t_name)
  xcol_name <- readline('컬럼명 입력')
  xdata <- table[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) )
}

stats()
----------------------------
  
stats <- function() {
  fname <- file.choose()
  fname
  
  table <- read.csv(fname, header =T)
  
  #t_name <- readline('테이블명 입력')
  #table_name = get(t_name)
  xcol_name <- readline('컬럼명 입력')
  xdata <- table[,xcol_name]
  
  x <- c()
  for ( i in 1:length(xdata) ) {
    x[i] <- xdata[i]
  }
  cat('최대값', max(x), '\n',
      '최소값', min(x), '\n',
      '평균값', mean(x),'\n',
      '중앙값', median(x),'\n',
      '분산값', var(x) , '\n',
      '표준편차',sd(x) )
  
  boxplot(xdata , beside=T,horizontal=T)
}

stats()
setwd("d:\\data\\")
source("myfunc.R")

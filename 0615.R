aggregate(sal~job, emp, sum ) 
dept <- read.csv("dept3.csv",header = T)
x <- merge( emp, dept, by="deptno", all=T)
x


attach(x)


aggregate(sal~dname, x, mean, na.action = na.pass)
round(tapply(sal,dname,mean))

x2 <- aggregate(sal~dname, x, mean, na.action = na.pass)

x2
library(data.table)
data.table(x2$dname, round(x2$sal))


x3<- round(tapply(sal,dname,mean))
x3
barplot(x3, ylim=c(0,3000))

install.packages("googleVis")
library(googleVis)
library(data.table)


install.packages("lubridate")
library(lubridate)
year(emp$hiredate)

x2 <- 
attach(x)
x2 <- tapply(sal, list(job,loc),sum)
x2[is.na(x2)] <- 0       
x2       


barplot(x2, beside=T,legend=rownames(x2))

line <- read.csv("1-4호선승하차승객수.csv", header=T)

line





t1 <- gvisMotionChart(line, idvar="line_no", timevar="time")

plot(t1)


rbind(
  emp[emp$deptno %in% c(10,20), c("ename","sal","deptno")],
  emp[emp$deptno == 10, c("ename","sal","deptno")]
)


x <- aggregate(sal~job,emp,sum)
names(x) <- c('직업','토탈값')
orderBy( ~ 직업, x)
rbind(
x,
c("토탈값:", sum(emp$sal))
)




x <- unique( 
  rbind(
    emp[ emp$deptno %in% c(10,20), c("ename","sal","deptno") ],
    emp[ emp$deptno ==10, c("ename","sal","deptno") ] 
  )
)
library(doBy)
orderBy( ~ deptno, x ) 





x <- aggregate(sal~job,emp,sum)


x5 <- rbind(
  aggregate(sal~job,emp,sum),
  c("토탈값:", sum(emp$sal))
)
names(x5) <- c('직업','토탈값')
orderBy( ~ 직업, x5)
x5


install.packages("dplyr")


library(dplyr)


x2 <- setdiff(
  emp[emp$deptno %in% c(10,20), c("ename","sal","deptno")],
  emp[emp$deptno == 10, c("ename","sal","deptno")]
)


library(doBy)
orderBy(~ename, x2)


intersect(
  emp[emp$deptno %in% c(10,20), c("ename","sal","deptno")],
  emp[ emp$deptno == 10, c("ename","sal","deptno")]
  )



jonesal <- emp[ emp$ename == 'JONES', 'sal']
jonesal

emp[emp$sal > jonesal, c("ename","sal")]

msal <- max(emp$sal)
msal
emp[emp$sal == max(emp$sal), c("ename","sal","job")]


univ <- read.csv("전국_대학별등록금통계_현황.csv", header = T)
head(univ)

univ[univ$등록금 == max(univ$등록금), c("학교명","등록금")]

k <- emp[emp$ename == 'KING', "empno"]
c
  library(data.table)
  unique(emp$mgr)
  data.table( 이름 = emp[!emp$empno %in% unique(emp$mgr), "ename"])
  
  
crloc <- read.csv("crime_loc.csv", header = T)
crloc

crloc[crloc$건수 == max(crloc$건수), "범죄"]

c1 <- crloc[crloc$장소 == "아파트",  c("범죄","건수")]
c1
c1[c1$건수 == max(c1$건수), ]


crd <- read.csv("crime_day.csv",header = T)
crd
attach(crd)
tapply(CNT,DAY,sum)
crd2 <- crd[trimws(crd$C_C)=="강력범죄",]
crd2
crdt<-aggregate(CNT~DAY, crd2, sum)

crdt[crdt$CNT == max(crdt$CNT),]





xx<-data.table(이름=emp$ename, 월급=emp$sal, 
             순위=rank(-emp$sal, ties.method="min")  ) 
library(doBy)
orderBy(~순위, xx )

library(dplyr)
x <- data.table(이름 = emp$ename, 월급 = emp$sal, 순위 = dense_rank(-emp$sal))
orderBy(~순위,x)


y <- emp[emp$job == 'SALESMAN',]
y1<- data.table(이름 = y$ename, 월급 = y$sal, 직업 = y$job, 순위 = dense_rank((-y$sal)))
orderBy(~순위, y1)

data.table(wcc2$암종, wcc2$환자수, 순위 = dense_rank(-wcc2$환자수))


cc2 <- read.csv("cancer2.csv", header=T)
cc2
wcc2 <- cc2[trimws(cc2$성별)=="여자" & trimws(cc2$암종) != '모든암',]
wcc2
wcc<-unique(wcc2[!is.na(wcc2$환자수),c("암종","환자수")])
head(wcc)
df <- data.table(wcc$암종, wcc$환자수, 순위 = dense_rank(-wcc$환자수))
df
orderBy(~순위, df)
wcc[wcc$환자수 == max(wcc$환자수),]
orderBy(~-환자수, wcc)

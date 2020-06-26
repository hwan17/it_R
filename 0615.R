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

line <- read.csv("1-4ȣ���������°���.csv", header=T)

line





t1 <- gvisMotionChart(line, idvar="line_no", timevar="time")

plot(t1)


rbind(
  emp[emp$deptno %in% c(10,20), c("ename","sal","deptno")],
  emp[emp$deptno == 10, c("ename","sal","deptno")]
)


x <- aggregate(sal~job,emp,sum)
names(x) <- c('����','��Ż��')
orderBy( ~ ����, x)
rbind(
x,
c("��Ż��:", sum(emp$sal))
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
  c("��Ż��:", sum(emp$sal))
)
names(x5) <- c('����','��Ż��')
orderBy( ~ ����, x5)
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


univ <- read.csv("����_���к���ϱ����_��Ȳ.csv", header = T)
head(univ)

univ[univ$��ϱ� == max(univ$��ϱ�), c("�б���","��ϱ�")]

k <- emp[emp$ename == 'KING', "empno"]
c
  library(data.table)
  unique(emp$mgr)
  data.table( �̸� = emp[!emp$empno %in% unique(emp$mgr), "ename"])
  
  
crloc <- read.csv("crime_loc.csv", header = T)
crloc

crloc[crloc$�Ǽ� == max(crloc$�Ǽ�), "����"]

c1 <- crloc[crloc$��� == "����Ʈ",  c("����","�Ǽ�")]
c1
c1[c1$�Ǽ� == max(c1$�Ǽ�), ]


crd <- read.csv("crime_day.csv",header = T)
crd
attach(crd)
tapply(CNT,DAY,sum)
crd2 <- crd[trimws(crd$C_C)=="���¹���",]
crd2
crdt<-aggregate(CNT~DAY, crd2, sum)

crdt[crdt$CNT == max(crdt$CNT),]





xx<-data.table(�̸�=emp$ename, ����=emp$sal, 
             ����=rank(-emp$sal, ties.method="min")  ) 
library(doBy)
orderBy(~����, xx )

library(dplyr)
x <- data.table(�̸� = emp$ename, ���� = emp$sal, ���� = dense_rank(-emp$sal))
orderBy(~����,x)


y <- emp[emp$job == 'SALESMAN',]
y1<- data.table(�̸� = y$ename, ���� = y$sal, ���� = y$job, ���� = dense_rank((-y$sal)))
orderBy(~����, y1)

data.table(wcc2$����, wcc2$ȯ�ڼ�, ���� = dense_rank(-wcc2$ȯ�ڼ�))


cc2 <- read.csv("cancer2.csv", header=T)
cc2
wcc2 <- cc2[trimws(cc2$����)=="����" & trimws(cc2$����) != '����',]
wcc2
wcc<-unique(wcc2[!is.na(wcc2$ȯ�ڼ�),c("����","ȯ�ڼ�")])
head(wcc)
df <- data.table(wcc$����, wcc$ȯ�ڼ�, ���� = dense_rank(-wcc$ȯ�ڼ�))
df
orderBy(~����, df)
wcc[wcc$ȯ�ڼ� == max(wcc$ȯ�ڼ�),]
orderBy(~-ȯ�ڼ�, wcc)
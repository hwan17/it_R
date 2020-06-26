library(data.table)

data.table(emp$ename,emp$sal,ifelse(emp$sal>=1500,'A','B'))

data.table(emp$ename,emp$job,
           ifelse(emp$job=='SALESMAN',4000,ifelse(emp$job=='ANALYST',6000,0) ))

emp$bonus <- ifelse(emp$job=='SALESMAN',4000,ifelse(emp$job=='ANALYST',6000,0) )
emp

emp[is.na(emp$comm), c("ename","comm")]

data.table(emp$ename, ifelse(is.na(emp$comm),'no comm', emp$comm))

is.na('ddd')

max(emp$sal)

max(emp[emp$job == 'SALESMAN', "sal"])


min(emp[emp$deptno == 20, "sal"])


aggregate(sal~job, emp, max)

aggregate(sal~deptno, emp, sum)

x <- aggregate(sal~deptno, emp, sum)
names(x) <- c('부서번호','토탈월급')
x

install.packages('doBy')


library('doBy')
library(doBy)
orderBy(~-토탈월급,x)

x <- aggregate(empno~job, emp, length)
names(x) <- c('직업','인원수')
orderBy(~-인원수,x)

table(emp$job)

x <- aggregate(sal~deptno+job, emp, sum)
x
orderBy(~deptno,x)


x <- aggregate(sal~format(as.Date(emp$hiredate),'%Y'), emp, mean)
names(x) <- c('입사년도','평균월급')
x$평균월급 <- trunc(x$평균월급)
x

aggregate(sal~job, emp, mean)


attach(emp)
tapply(sal,job,mean)

trunc(tapply(sal,job,mean))

x <- trunc(tapply(sal,job,mean))

barplot(x, main = "년도별 평균월급", col=rainbow(5),density=90)


attach(emp)
tapply(sal,list(job,deptno), sum)

x <- tapply(sal,list(job,deptno), sum)

x[is.na(x)] <- 0
x

  colnames(x)
  rownames(x)
  
barplot(x, col=rainbow(5),legend=rownames(x), beside=T, density = 90)

rm(x)

x<-tapply( emp$sal, emp$job, sum)
x
pie(x,col=rainbow(5))


install.packages("plotrix")
library(plotrix)
pie3D(x = x,explode=0.1,labels=rownames(x))

x2 <- aggregate(sal~job, emp, sum)
x2
round(x2$sal/sum(emp$sal)*100,1)
pct <- round(x2$sal/sum(emp$sal)*100,1)
job_label <- paste(x2$job, ':', pct, '%')
job_label
pie3D(x = x,explode=0.1,labels=job_label)

x4 <- tapply(emp$empno,emp$deptno,length)
x3 <- aggregate(empno~deptno,emp,length)
x3
x4
round(x3$empno/sum(x3$empno)*100,1)
pct2 <- round(x3$empno/sum(x3$empno)*100,1)
count_lb <- paste('부서번호 : ',x3$deptno, ':', pct2, '%')
count_lb
pie3D(x4,explode = 0.1,labels=count_lb)

x4

str(x3)


dept <- read.csv("c:\\data\\dept3.csv", header = T)
dept

x <- merge(emp, dept, by="deptno")
x
x[,c('ename','loc')]


x[,c('loc','ename')]

x[x$loc=='DALLAS', c('ename','sal','loc')]

x[is.na(x$comm), c('ename','loc','comm')]


x <- merge(emp, dept, by="deptno", all.y=F)
x


x <- merge(emp, dept, by="deptno", all=T)


x


x <- merge(emp, emp, by.x = "mgr", by.y = "empno")

x[x$sal.x>x$sal.y, c("ename.x","sal.x","ename.y","sal.y")]

install.packages("igraph")
library(igraph)

a <- x[,c("ename.x","ename.y")]
b <- graph.data.frame(a, directed = T)
plot(b)


install.packages("googleVis")
library(googleVis)

a <- merge(emp,emp, by.x="empno",by.y="mgr", all.y=T)



org <- gvisOrgChart(a, idvar="ename.y",parentvar="ename.x",
                    
                    options=list(width=600, height=250, size='middle',allowCollapse=T))



plot(org)

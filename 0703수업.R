library(class)
wbcd <- read.csv("d:\\data\\csv\\wisc_bc_data.csv", header=T, stringsAsFactors=FALSE)
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))

wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]
wbcd2 <- wbcd_shuffle[-1]
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x)) )
}

wbcd_n <- as.data.frame(lapply(wbcd2[2:31],normalize))

train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

result1 <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_label, k=21)

result1
result2 <- kmeans(wbcd_test, 2)
result2$cluster
result3 <- ifelse(wbcd_test_label == 'Maliganant',1,2)
result3

sum(result3 == result2$cluster)


ave(1:3)


emp <- read.csv("d:\\data\\csv\\empcn.csv")
ave(emp$sal,emp$deptno)
x
emp$avgsal <- ave(emp$sal, emp$deptno, FUN=function(x) mean(x))
emp[,c("ename","sal","deptno","avgsal")]

#문제 287. 이름과 월급, 직업 자기가 속한 평균 월급을 출력

ave(emp$sal,emp$job)
ave(emp$sal, emp$deptno, FUN=function(x) mean(x))
emp$avgsal2 <- ave(emp$sal, emp$job, FUN=function(x) mean(x))
emp[,c("ename","sal","job","avgsal2")]
emp[emp$job == x,"ename","sal","deptno","avgsal"]

#문제288. 사원테이블에 결측치가 얼마나 있는지
colSums(is.na(emp))


#문제289.
if (is.na(emp$comm)==T) {
ave(emp$sal, emp$deptno, FUN=function(x) mean(x))[is.na(emp$comm)]
}
emp$comm[is.na(emp$comm)]=ave(emp$sal, emp$deptno, FUN=function(x) mean(x))[is.na(emp$comm)]
emp
emp$comm[emp$comm==800,]

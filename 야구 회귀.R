setwd("d:\\data\\csv\\")
Sys.setlocale("LC_ALL","C") # 강제 언어 삭제
data1 =  read.csv(file.choose(), header = T, sep=",",encoding = "UTF-8",stringsAsFactors = T) #file read 시, UTF-8로 인코딩
Sys.setlocale("LC_ALL","Korean") # 언어 다시 한글로


str(data1[,- c(1,22,26,27,28,25,35)])
datan <- data1[,- c(1,22,23,25,26,27,28,35)]
head(data1)
str(datan)
normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data1_n <- as.data.frame(lapply(datan, normalize))
summary(data1_n)

data1
str(data1)
attach(data1)
baselm <- lm(data1$salary~age+G+PA+AB+R+H+X2B+X3B+HR+TB+RBI+SB+CS+BB+HBP+GB+SO+GDP+BU+fly+war+hand2+cp+tp+X1B+FBP+avg+OBP+SLG+OPS+YAB+YOPS,data = data1)
summary(baselm)

head(data1)
unique(data1$hand2)
unique(data1$tp)
unique(data1$cp)


library(car)
data(data1,package = "MASS")

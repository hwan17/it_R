Sys.setlocale("LC_ALL","C") # ���� ��� ����

data1 =  read.csv(file.choose(), header = T, sep=",",encoding = "UTF-8") #file read ��, UTF-8�� ���ڵ�

Sys.setlocale("LC_ALL","Korean") # ��� �ٽ� �ѱ۷�


data1
summary(data1)

setwd("d:\\data\\csv")
pj <- read.csv("pj2.csv",stringsAsFactors = T)
nrow(pj)
summary(pj)
str(pj)

head(pj)

nrow(pj[,"SKU_number"])
length(unique(pj$SKU_number))
library(doBy)
table(pj$SKU_number,pj$SoldCount)
pj[pj$SoldCount==73,]


length(unique(data1$batter_name))
nrow(data1)
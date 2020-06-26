
library(e1071)
flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
flu <- flu[-1]
#colnames(movie) <- c("age","gender","job","marry","friend","m_type")
#nrow(flu)
#train <- movie[1:39,]
#ncol(flu)
model <- naiveBayes(flu[ ,1:4], flu[,5] , laplace=0)
#test <- read.csv("flu_patient.csv", header=T, stringsAsFactor=F )

# 윈도우 탐색기로 테스트환자를 불러옵니다.
#fname <- file.choose()
#test <- read.csv(fname, header=T, stringsAsFactor=F )


chillsv <-readline("오한 [Y/N] ")
nosev <-readline("콧물 [Y/N] ")
headv <-readline("두통 [STRONG/MILD/NO] ")
fevev <-readline("열 [Y/N] ")
test <- data.frame(chills=chillsv,runny_nose=nosev,headache=headv,fever=fevev)
result <- predict( model, test, type = "raw")
result
paste("환자가 독감일 확률은",round(result[2]*100,1),"% 입니다.")
test

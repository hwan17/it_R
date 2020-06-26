Riper_func <- function() {
set.seed(11)
setwd("d:\\data")
mushroom<-read.csv("mushrooms.csv", stringsAsFactors = T)
#dim(mushroom)

train_cnt <- round( 0.75 * dim(mushroom)[1])
train_index <- sample(1:dim(mushroom)[1], train_cnt, replace=F)

mushroom_train <- mushroom[train_index, ]
#mushroom_test <- mushroom[-train_index, ]

library(RWeka)
fname <- file.choose()  
mushroom_test <- read.csv(fname, header=T, stringsAsFactor=T )    
model2 <- JRip(type~ ., data=mushroom_train)
result2 <- predict( model2, mushroom_test[, -1] )
print(ifelse(result2=='edible',"식용입니다.","식용이아닙니다."))
}
Riper_func()

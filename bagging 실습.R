
setwd("d:\\data\\csv")
#install.packages("caret") 

library(caret)
credit <- read.csv("credite.csv",stringsAsFactors = T)


library(ipred)
set.seed(300)
mybag <- bagging(default~., data = credit, nbagg=50)
mybag

credit_pred <- predict(mybag,credit)
table(credit_pred,credit$default)
prop.table(table(credit_pred==credit$default))



library(adabag)
set.seed(300)
m_adaboost <- boosting(default~., data=credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion
table(p_adaboost$class, credit$default)

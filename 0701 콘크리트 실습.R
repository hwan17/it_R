concrete <- read.csv("d:\\data\\csv\\concrete.csv")
head(concrete)
str(concrete)
hist(concrete$strength)
# 머신러닝 모델의 학습이 잘 될것이라고 예상

# 결측치 확인
colSums(is.na(concrete))
# 이상치 확인



# 정규화
normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

con_n <- as.data.frame(lapply(concrete, normalize))

summary(con_n)
concrete_train <- con_n[1:773, ]
concrete_test <- con_n[774:1030, ]


install.packages("neuralnet")
library(neuralnet)
attach(con_n)
con_model <- neuralnet(formula = strength~., data = concrete_train)
con_model
plot(con_model)

model_results <- compute(con_model, concrete_test[1:8])
head(model_results)
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)


concrete_model2 <- neuralnet(formula=strength ~.,
                             data =concrete_train , hidden=c(6,4) )
plot(concrete_model2)
concrete_model2
model_results <-compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results$net.result
cor(predicted_strength2, concrete_test$strength)


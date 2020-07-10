setwd("D:\\data\\csv")
getwd()


# csv 파일 불러옴(이전에 안에 있는 한글 전부 영어로 다 바꿈)
body <- read.csv("kbody2.csv", header = F,stringsAsFactors = T)


# 라이브러리 불러옴
library(e1071)
library(caret)

# na값 제거
body1 <- na.omit(body)


# 컬럼명 줌
colnames(body1) <- c("gender", "age", "height", "chest", "heory", "bae", "ass", "kyeo",
                     "face_vertical", "head", "bone", "body_fat", "body_water",
                     "protein", "mineral", "body_fat_per", "bae_fat_per", "work", "bae_fat_test",
                     "work_test")


# 랜덤시드 생성
set.seed(12345)


# 셔플
body_ran <- body1[order(runif(12894)), ]


# 트레이닝셋 80%
body_train <- body_ran[1:10314, ]


# 테스트셋 20%
body_test <- body_ran[10315:12894, ]

str(body_train)
# 선형SVM 훈련
body_svm <- svm(work_test~., data = body_train, kernel="rbf")
body_svm

ctrl <- trainControl(method = "cv", number=10)
bagctl <- bagControl(fit = svmBag$fit, predict = svmBag$pred,aggregate = svmBag$aggregate)

set.seed(300)

svmbat <- train(work_test ~., data = body_train,"bag",trControl=ctrl,bagControl=bagctl)

#fit <-train(work_test ~ ., data = body_train, method = 'svmRadial', tuneGrid = data.frame(sigma=0.0107249, C=1))
str(svmbat)
# 모델 테스트
p <- predict(body_svm, body_test, type="class")
p
table(p, body_test[, 20])


# 분류 결과 확인
mean(p == body_test[, 20])


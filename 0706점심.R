setwd("d:\\data\\csv")
sre <- read.csv("sms_results.csv",stringsAsFactors = T)
nrow(sre)
str(sre)
head(sre)

library(gmodels)
CrossTable(sre$actual_type,sre$predict_type,prop.chisq = F,prop.c = F,prop.r = F, prop.t = F)
CrossTable(sre[,2],sre[,1],prop.chisq = F,prop.c = F,prop.r = F,prop.t = F)

ct <- CrossTable(sre$actual_type,sre$predict_type,prop.chisq = F,prop.c = F,prop.r = F, prop.t = F)
ct

# 정확도
pra=(ct$t[1]+ct$t[4])/sum(ct$t)
t1 <- ct$t[1]
t2 <- ct$t[3]
t3 <- ct$t[2]
t4 <- ct$t[4]

install.packages("vcd")
library(vcd)
table(sre$actual_type,sre$predict_type)
Kappa(table(sre$actual_type,sre$predict_type))


(t1+t2)/sum(ct$t)#ham1

(t1+t3)/sum(ct$t)#ham2

(t1+t2)/sum(ct$t)*(t1+t3)/sum(ct$t) #ham
(1-(t1+t2)/sum(ct$t))*(1-(t1+t3)/sum(ct$t)) #spam

pre = (t1+t2)/sum(ct$t)*(t1+t3)/sum(ct$t)+(1-(t1+t2)/sum(ct$t))*(1-(t1+t3)/sum(ct$t))
pre
(pra-pre)/(1-pre)


library(caret)
sens <- sensitivity(sre$predict_type,sre$actual_type,positive = "spam")
spec <- specificity(sre$predict_type,sre$actual_type,negative = "ham")
prec <- posPredValue(sre$predict_type,sre$actual_type,positive = "spam")


install.packages("ROCR")
library(ROCR)
head(sre) #3번째 컬럼과 4번째 컬럼의 확률을 확인
pred <- prediction(predictions = sre$prob_spam, labels = sre$actual_type)
pred



# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
perf
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)
# add a reference line to the graph
# 대각선 출력 
abline(a = 0, b = 1, lwd = 2, lty = 2)
# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

2*(prec*sens)/(prec+spec)

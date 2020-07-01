inputs<-matrix(c(0,0,1,0,0,1,1,1),nrow=4,byrow=T) 
target<-matrix(c(0,0,0,1),nrow=4)
target
new_inputs<-cbind(matrix(c(-1,-1,-1,-1),nrow=4),inputs)
new_inputs # input 데이터
#pcs <- matrix(c(0,0,0,0),nrow=1) # 데이터와 가중치 곱의 합 저장
#pcs
w <-matrix(c(0.2,0.4,0.2), nrow = 1,byrow = T)
w2 <- matrix(c(0,0,0), nrow = 1,byrow = T)

lr = 0.05
#cnt1 = 0
#cnt2 = 0
#end = 0
#while (1){
  repeat{
    for (i in 1:4){
      xisum <- sum(new_inputs[i,]*w)
      er <- ifelse(xisum>=0,1,0)
      
      if ( (target[i]-er) != 0 ) {
        for (j in 1:3) {
          w[j] = w[j] + lr*new_inputs[i,j]*(target[i]-er)  
        }
        #w2 = w
        i=0
        break
      
      }
    }
    if (i == 4){
      break
    }
  }
  #break
#}
w
cnt1
new_inputs[1,]*w
sum(new_inputs[1,]*w)
ifelse(sum(new_inputs[1,]*w)>=0,1,0)


ste<-ifelse(k>=0,1,0) # 활성화 함수


and_pcn(new_inputs,target,0.05)


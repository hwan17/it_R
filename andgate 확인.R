inputs<-matrix(c(0,0,1,0,0,1,1,1),nrow=4,byrow=T) 
target<-matrix(c(0,0,0,1),nrow=4) # target

new_inputs<-cbind(matrix(c(-1,-1,-1,-1),nrow=4),inputs)
new_inputs # input 데이터
new_inputs[2,2]
w <-matrix(c(0.3,0.4,0.1), nrow = 1,byrow = T)
lr = 0.05 #learning rate

k=0
cnt = 0
repeat{
  for (i in 1:4){
    xisum <- sum(new_inputs[i,]*w)
    er <- ifelse(xisum>=0,1,0) # 활성화 함수
    if ( (target[i]-er) != 0 ) { # 오차 판별
      for (j in 1:3) { # 오차가 생기면 역전파 가중치 변경
        w[j] = w[j] + lr*new_inputs[i,j]*(target[i]-er)  
      }
      i=0
      print(w)
      break # 바뀐 가중치로 처음부터 확인 
    }
  }
  if (i == 4){ # 마지막까지 이상없이 돌면 끝 
    break
  }
}



w





#----------
i=0
repeat{
  if (i==5){
    break
  }
  i=i+1
  print(i)
}

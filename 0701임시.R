inputs1 <- matrix(c(0,0,1,0,0,1,1,1), nrow=4, ncol=2, byrow=T)
targets1 <- matrix(c(0,0,0,1), ncol=1)
a <- matrix(c(1,1,1,1), ncol=1)
new_input <- cbind(a,inputs1)
w <-  matrix(c(runif(3)), ncol=1)
k <- new_input%*%w
step <- function(x) { ifelse(x>=0,1,0) }
step(k)
tkf <- targets1-step(k)
for (k in 1:5){
for ( i in 1:4) {
  for (j in 1:3) {
    w[j] <- w[j]+0.05*new_input[i,j]*tkf[i]
  }
  cat("\n row:",i,w)
}
  cat("\n ",k)
}
#row: 1 0.7672032 0.5960613 0.1026152
#row: 2 0.7172032 0.5460613 0.1026152
#row: 3 0.6672032 0.5460613 0.05261521
#row: 4 0.6672032 0.5460613 0.05261521

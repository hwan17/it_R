inputs1 <- matrix(c(0,0,1,0,0,1,1,1), nrow=4, ncol=2, byrow=T)
targets1 <- matrix(c(0,0,0,1), ncol=1)
lr = 0.05

and_pcn <- function(inputs1,targets1,lr){
  #a <- matrix(c(-1,-1,-1,-1), ncol=1)
  a <- matrix(c(1,1,1,1), ncol=1)
  new_input <- cbind(a,inputs1)
  w <-  matrix(c(runif(3)), ncol=1)
  #w <- matrix(c(0.3,0.4,0.1))
  for (l in 1:15){
    k <- new_input%*%w
    step <- function(x) { ifelse(x>=0,1,0) }
    step(k)
    tkf <- targets1-step(k)
    for ( i in 1:4) {
      #if (tkf[i]!=0){
        for (j in 1:3) {
          w[j] <- w[j]+lr*new_input[i,j]*tkf[i]
        }
      cat("\n row:",i,w)
      
    #}
    cat("\n ",l)
    }
  }
}


and_pcn(inputs1,targets1,lr)

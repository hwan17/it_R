#1
inputs1 <- matrix(c(0,0,1,0,0,1,1,1), ncol=2, byrow =T)
inputs1
#2
targets1 <- matrix(c(0,0,0,1), ncol = 1)
targets1
#3
w <- matrix( round(runif(3),3), ncol=1)
w


#4
new_input <- cbind(matrix(c(1,1,1,1),ncol=1),inputs1)
new_input


#5
k <- new_input%*%w
#¤¤k´Â ÀÔ·Â°ª°ú °¡ÁßÄ¡ °öÀÇ ÃÑÇÕ

#6
step <- function(x) { ifelse(x>=0,1,0) }
step(k)

#7
tkf <- targets1-step(k)
tkf
targets1

#8
for (i in 1:4){
  cat("\n",i)
}

#9
for (i in 1:4){
  for (j in 1:3){
    w[j] <- w[j] + 0.05*new_input[i,j]*tfk[i]
  }
}




#269

relu <-function(x){
  ifelse(x>0,x,0)
}
x <-seq(-10,10,0.01)
x
plot(relu(x))


#270

fstep <- function(x){
  ifelse(x>=0,1,0)
}
fstep(-.1)
fstep(1.4)
x <- seq(-5,5,.01)
x
fstep(x)
plot(fstep(x))


#271

fsgm <- function(x) {
  return (1/(1+exp(-x)))
}

fsgm(x)
plot(fsgm(x))
plot(x,fsgm(x))

library(parallel)
a <- rnorm(1000000)
a
system.time(a <- rnorm(1000000))

l1 <- rnorm(1000000)



system.time(l1 <- rnorm(1000000)) 



system.time(a<-unlist(mclapply(1:2, function(x) {
  
  rnorm(1000000) } , mc.cores=1 )))



library(foreach)

system.time(l4<- foreach(i =1:400, .combine='c')
            
            %do% rnorm(250000))




library(doParallel)

registerDoParallel(cores=4)

system.time(l4<- foreach(i =1:400, .combine='c')
            
            %do% rnorm(250000))



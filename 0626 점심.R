
plot(k_index$k_rate ~ k_index$kospi, col="blue")
k_index <- read.csv("csv\\K_index.csv", header=T,stringsAsFactors=F)
model_s <- lm( k_index$k_rate ~ k_index$kospi, data=k_index)
lm()
abline( model_s, col="red")

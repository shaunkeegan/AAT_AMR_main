

-1 * log(0.885)/4

qf <- 0.96
qn <- 0.98
feed.cyc <- 4
p <- 0

mat <- matrix(1:2000, ncol = 2)
mat[] <- NA
pmat <- seq(0,0.999,0.001)

for (i in 0:999){
  p <- i/1000
  
  val1 <- (1-p)* qf * qn^feed.cyc
  val2 <- -1 *log((1-p)* qf * qn^feed.cyc)/feed.cyc
  
  mat[i+1,1] <- val1
  mat[i+1,2] <- val2
}


-1 *log((1-p)* qf * qn^feed.cyc)/feed.cyc



par(mfrow=c(1,2))
plot(mat[1:1000,1] ~ pmat, lwd = 2, type = 'l', ylab =   "Survival Probability", xlab = "Insecticide Proportion" )
plot(mat[1:1000,2] ~ pmat, lwd = 2, type = 'l', ylab =   "Mortality Rate", xlab = "Insecticide Proportion" )

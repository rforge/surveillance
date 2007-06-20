library(surveillance)
library(boot)

#In case of normal distribution compute h & k so we get a desired value
kh <- find.kh(ARLa = 500, ARLr = 7, verbose=FALSE)
xcusum.arl(k=kh$k,h=kh$h,mu=0,r=50,sided="one")

######################################################################
#Matrix way to generate things (MUCH FASTER!!) -- instead of algo.cusum
######################################################################
size=2000
noReps=1000
h <- 2.5#kh$h
k <- 3#kh$k
ma<- 1
#Density and cdf of the desired density (norm, exp, etc)
rdist <- rexp
pdist <- pexp
#Simulate the data
X <- matrix(rdist(size*noReps,ma),nrow=noReps)
#Run the Cusum on each
S <- matrix(0,nrow=noReps,ncol=size+1)
for (i in 2:(size+1)) {
  S[,i] <- pmax(0,S[,i-1] + X[,i-1] - k)
}
alarm <- S>h

#Detect the first alarm instance in each time series
runmax <- function(x) {
  wm <- which.max(x)-1; if (wm>0) return(wm) else return(size)
}
N <- apply(alarm,MARGIN=1,runmax)

#Compute total hazards and the Y estimator
hazard <- t(apply(1-pdist(k+h-S,ma),MARGIN=1,cumsum))
Y <- hazard[cbind(1:noReps,N)]

######################################################################
# Do the calculations
######################################################################
n <- length(N)

#Check correlation -- the higher the better
cor(N,Y)

#mean(N)
#var(N)
#sum((N-mean(N))^2)/(length(N)-1)
#var(N)/n

#Compare var(N) and E(N)^2, because the paper states they are approximately
#equal
var(N)
mean(N)^2

#Total hazard estimator -- the manual way
a <- -cov(N,Y)/var(Y)
a
#Exact (manual...)
-(sum(N*Y)-length(N)*mean(N)*mean(Y))/(sum(Y^2)-length(N)*mean(Y)^2)

#Raw
mean(N)
#Better estimator
delta <- N+a*(Y-1)
mean(delta)

#Standard deviations
sqrt(var(N)/n)
sqrt(var(delta)/n)


#Use a regression approach to determine the control variate estimator
#c.f Givens & Hoeting, p.173
m <- lm(N ~ Y)
coef(m)
mean(delta)
sum(coef(m))

#Prediction by linear model (c.f. p.173 in Givens & Hoeting)
predict(m,data.frame(Y=1),se.fit=TRUE)
mean(N) + a*(mean(Y)-1)

######################################################################
# Compute the random hazard control variate delta.hat based
# on the N and Y vector
######################################################################
delta.hat <- function(data,index,...) {
  N <- data[index,1]
  Y <- data[index,2]
  m <- lm(N ~ Y)
  #Prediction by linear model (c.f. p.173 in Givens & Hoeting)
  predict(m,data.frame(Y=1))#,se.fit=TRUE)
}

mse.hat <- function(data,index,ARL,...) {
  N <- data[index,1]
  Y <- data[index,2]
  m <- lm(N ~ Y)
  #Prediction by linear model (c.f. p.173 in Givens & Hoeting)
  delta <- predict(m,data.frame(Y=1))#,se.fit=TRUE)
  (delta-ARL)^2
}


##Check by bootstrap
NY <- cbind(N,Y)
b <- boot(NY, delta.hat, R=999, stype="i")

#MSE = Bias^2 + Var
#If true value not known we estimate it by the mean of all 1000 values
#(mean(c(b$t,b$t0)) - b$t0)^2 + var(b$t)
#If it is known we can do better
(6.39 - b$t0)^2 + var(c(b$t0,b$t))
(53.6 - b$t0)^2 + var(c(b$t0,b$t))
(225.4 - b$t0)^2 + var(c(b$t0,b$t))

b2 <- boot(NY, mse.hat, R=999, stype="i",ARL=225.4)
mean(c(b2$t0,b2$t))


##Some simple data

x <- c(4,3,2,1,3,2,5,6,7,3,2,1,1,1,2,2.5,5,6,7,1,0,0,-1,-1,0,1,3,2)-2
k <- 1
plot(x)

S <- numeric(length(x)+1)
for (i in 1:length(x)) {
  S[i+1] <- max(0,S[i] + x[i]-k)
}
plot(0:length(x),S)
lines(c(0,1e99),c(3,3),col=2)


#Stream of Bernoulli RVs
pi0 <- 0.05
gamma <- 2
pi1 <- gamma*pi0

set.seed(123)
y <- rbinom(1e5,size=1,p=pi0)
#Number of observations between the (i-1)th and ith failure
b <- diff(which(y == 1))

#Compare empirical with geometric
hist(b,prob=T,breaks=seq(-0.5,max(b)+0.5,by=1))
points(1:max(b),dgeom(0:(max(b)-1),pi0),col=2)

#If pi0 is small we can use the transformation to exponential
#as in Anscombe & Page
x <- (b-1)*log((1-pi0)/(1-pi1))
hist(x,nclass=100,prob=T)

theta0 <- 1/(gamma-1)
lines(x.grid <- seq(0,max(x),length=1000),dexp(x.grid,theta0),col=2)

sim.cusum <- function(size,pi0,a=1,gamma=2) {
  #Constants
  pi1 <- gamma*pi0
  #Saved y values -- so far none
  y <- NULL
  noAlarm <- TRUE
  noSback2zero <- TRUE
  
  while (noAlarm | noSback2zero) {
    #Generate data and append to existing ones until we have at least two
    #malformations
    repeat {
      y <- c(y,rbinom(size,size=1,p=pi0))
      if (sum(y)>=2) break
    }
    #Number of observations between the (i-1)th and ith failure
    b <- diff(which(y == 1))
    x <- (b-1)*log((1-pi0)/(1-pi1))

  
    #Do the cusum
    S <- numeric(length(x)+1)
    for (i in 1:length(x)) {
      S[i+1] <- max(0,S[i] - x[i] + log(gamma))
    }

    #Time of first alarm
    first <- which.max(c(S[-1] > a,TRUE))
    noAlarm <- first > length(x)
    #First reset after first alarm
    if (!noAlarm) {
      reset <- which.max(c(S[-c(1:(first+1))],0)==0)-1 + first+1
      noSback2zero <- reset > length(x)
      #print(reset)
    }
  }
  

  #Count the number of alarms the modified procedure sounds
  alarms <- sum(S[(first:reset)+1] > a)

  #print(which.max(cumsum(y) == (first+1) ))
  #print(which.max(cumsum(y) == (reset+1) ))
  return(c(first=first,M2=reset,alarms=alarms,req.size=length(y)))
}

sim.cusum(size=1e3,pi0=0.1,a=6,gamma=1.1)

#a <- replicate(1000,sim.cusum(size=1e3,pi0=0.1,a=4,gamma=2))
a <- sapply(1:1000,function(i) {print(i);sim.cusum(size=1e3,pi0=0.1,a=4,gamma=2)})

mean(a[3,])/mean(a[2,]) * 10^5

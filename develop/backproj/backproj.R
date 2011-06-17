######################################################################
# Experimental implementation of the backprojection method by Becker
# (1991). Useful in outbreak situations
######################################################################

######################################################################
#Replace NaN or is.infinite values with zero.
#Good against division by zero problems.
######################################################################
naninf2zero <- function(x) {x[is.nan(x) | is.infinite(x)] <- 0 ; return(x)}


######################################################################
# Single step of the EMS algorithm by Becker et al (1991)
#
# Parameters:
#  lambda.old - vector of current value of the rates of length T
#  N.old      - matrix of current value of the complete data
#  Y          - vector observed value of length T
#  k          - smoothing parameter, needs to be an even number
######################################################################

em.step.becker <- function(lambda.old, Y, dincu, pincu, k=8) {
  #k needs to be divisible by two
  if (k %% 2 != 0) stop("k needs to be even.")

  #Initialize
  T <- length(Y)
  
  #Define new parameters
  phi.new <- lambda.new <- 0*lambda.old
  
  #EM step. Problem that some of the sums can be zero if the incubation
  #distribution has zeroes at d=0,1,2
  for (t in 1:T) {
    #Calculate sum in (3a) of Becker (1991)
    sum3a <- 0
    for (d in 0:(T-t)) {
      sum3a <- sum3a + Y[t+d] * naninf2zero(dincu(d) / sum(sapply(1:(t+d),function(i) lambda.old[i]*dincu(t+d-i))))
    }
    phi.new[t] <- naninf2zero(lambda.old[t]/pincu(T-t)) * sum3a
  }
  
  #Smoothing step
  if (k>0) {
    w <- choose(k,0:k)/2^k
    for (t in 1:T) {
      i.sub <- t+(0:k)-k/2
      goodIdx <- i.sub %in% 1:T
      w.sub <- w[goodIdx]/sum(w[goodIdx])
      lambda.new[t] <- sum(w.sub * phi.new[i.sub[goodIdx]])
    }
  } else { #no smoothing
    lambda.new <- phi.new
  }
  
  #Done.
  return(list(lambda=lambda.new))
}

######################################################################
# This is the iterator function for the backprojection method by
# calling em.step.becker.
#
# Parameters:
#  Y -
#  k - smoothing parameter
#  eps - relative convergence criteration
#  iterm.max - max number of iterations
#  verbose - extra output
#  lambda0 - start value for lambda, default: uniform
#  hookFun - hook function
#
# Returns:
#  vector of lambdas.
######################################################################

backproj.becker <- function(Y,dincu,pincu,k=8,eps=1e-5,iter.max=250,verbose=TRUE,lambda0=rep(sum(Y)/length(Y),length(Y)),hookFun=function(Y,lambda,...) {},...) {
  #Iteration counter and convergence indicator
  i <- 0
  stop <- FALSE
  res <- list(lambda=lambda0)
  
  #Loop until stop 
  while (!stop) {
    #Add to counter
    i <- i+1
    lambda.i <- res$lambda
    res <- em.step.becker(lambda.old=lambda.i,Y=Y,dincu=dincu,pincu=pincu,k=k)
    
    #check stop
    if (verbose) {
      cat("Convergence criterion @ iteration i=",i,": ", abs(sum(res$lambda) - sum(lambda.i))/sum(lambda.i),"\n")
    }
    stop <- abs(sum(res$lambda) - sum(lambda.i))/sum(lambda.i) < eps | (i>iter.max)

    #Hook
    hookFun(Y,res$lambda,...)
  }
  #Done
  return(res$lambda)
}


plotIt <- function(Y,lambda,...) {
  par(las=2)
  T <- length(Y)
  plot(1:T,Y,type="h",ylab="Cases",lwd=2,xaxt="n",xlab="",...)
  lines(1:T+0.3,lambda,type="h",col="gray",lty=1,lwd=2)  
  axis(1,at=1:T,label=1:T,cex.axis=0.7)
  legend(x="topleft",c(expression(Y[t]),expression(lambda[t])),col=c(1,"gray"),lty=c(1,1),lwd=2)

  invisible()
}

#T <- max(times) #lets say the last days are not good yet
#Y <- table(factor(times,levels=1:max(times)))[1:T]
#T <- length(Y)

######################################################################
#Simulated outbreak
######################################################################

#Incubation time distribution vector (support starts at zero!)
inc.pmf <- c(0,pgamma(1:25,15,1.4) - pgamma(0:24,15,1.4))
inc.cdf <- cumsum(inc.pmf)

#Wrap above array within PMF and CDF functions with discrete support.
dincu <- function(x) {
  notInSupport <- x<0 | x>=length(inc.pmf)
  #Give index -1 to invalid queries
  x[notInSupport] <- -1
  return(c(0,inc.pmf)[x+2])
}
inc.cdf <- cumsum(inc.pmf)
pincu <- function(x) {
  x[x<0] <- -1
  x[x>=length(inc.cdf)] <- length(inc.cdf)-1
  return(c(0,inc.cdf)[x+2])
}
rincu <- function(n) {
  sample(0:25, size=n, replace=TRUE, prob=inc.pmf)
}


barplot(inc.pmf,names.arg=0:25)

#Simulate outbreak starting at time t0 of length l
t0 <- 23
l <- 10

#Sample time of exposure and length of incubation time
n <- 1e3
exposureTimes <- t0 + sample(x=0:(l-1),size=n,replace=TRUE)
symptomTimes <- exposureTimes + rincu(n)

X <- table( factor(exposureTimes,levels=1:max(symptomTimes)))
Y <- table( factor(symptomTimes,levels=1:max(symptomTimes)))

plot(1:length(Y),Y,type="h",xlab="",lwd=2,ylim=c(0,max(X,Y)))
lines(1:length(Y)+0.2,X,col="gray",type="h")

#Do the EM looping
#bp.k0 <- backproj.becker(Y=Y,k=0,dincu=dincu,pincu=pincu,eps=1e-5)
bp.k0 <- backproj.becker(Y=Y,k=0,dincu=dincu,pincu=pincu,eps=1e-6,hookFun=plotIt,ylim=c(0,max(X,Y)))
lines(1:length(Y),X,col=2,type="h")

incu.sample <- rincu(1e5)
delta.star <- quantile(incu.sample,p=(1:length(symptomTimes))/length(symptomTimes),type=3)
exposureTime.hypothetical <- symptomTimes -  delta.star

plot(1:length(Y),Y,type="h")
other <- table(factor(exposureTime.hypothetical,levels=1:max(symptomTimes)))
lines(1:length(Y)+0.2,X,col=2,type="h")
lines(1:length(Y)+0.4,other,type="h",col="green")

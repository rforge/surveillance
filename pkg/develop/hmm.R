polio <- read.table("polio.asc",heade=TRUE)
plot(polio$t,polio$y,type="l")
polio <- data.frame(y=polio$y,t=polio$t)
library(msm)

#Fit a two component hmm
hmmNS <- msm(y ~ t, data=polio,
           qmatrix = rbind( rep(1/2,2), rep(1/2,2)),
           hmodel = list(hmmPois(rate=0.4),hmmPois(rate=3.4)),
           hcovariates = list( ~ 1, ~1)
           )


#mean(polio$y[polio$y <= mean(polio$y)])
crudeMean <- as.numeric(with(polio,tapply(y, y > mean(y), mean)))
polio <- with(polio,cbind(polio,cos1t=cos(2*pi*t/12),sin1t=sin(2*pi*t/12)))

hmmSt <- msm(y ~ t, data=polio,
           #Two state HMM
           qmatrix = rbind( rep(1/2,2), rep(1/2,2)),
           #y|x \sim Po( \mu[t] ) with some initial values
           hmodel = list(
             hmmPois(rate=crudeMean[1]),
             hmmPois(rate=crudeMean[2])
             ),
            #Models for \log \mu_t^1 and \log \mu_t^2
            hcovariates = list(
              ~1+t+cos1t+sin1t,
              ~1+t+cos1t+sin1t)
            )


hmmS <- msm(y ~ t, data=polio,
           #Two state HMM
           qmatrix = rbind( rep(1/2,2), rep(1/2,2)),
           #y|x \sim Po( \mu[t] ) with some initial values
           hmodel = list(
             hmmPois(rate=crudeMean[1]),
             hmmPois(rate=crudeMean[2])
             ),
            #Models for \log \mu_t^1 and \log \mu_t^2
            hcovariates = list(
              ~1+cos1t+sin1t,
              ~1+cos1t+sin1t)
            )


#Model selection
AIC(hmmNS)
AIC(hmmSt)
AIC(hmmS)

AIC(hmmNS,k=log(nrow(polio)))
AIC(hmmSt,k=log(nrow(polio)))
AIC(hmmS,k=log(nrow(polio)))


pmatrix.msm(hmmNS,t=1)
pmatrix.msm(hmmS,t=1)


hmm
hmm$estimates
hmm$hmodel
coef(hmm)
summary(hmm)
summary(hmm)
summary.msm(hmm)

#Extract transition probability matrix


#Stationary distribution
P <- pmatrix.msm(hmmNS,t=1)
pi <-matrix(c(1,1),1,2) %*% solve(diag(c(1,1))-P + matrix(c(1,1,1,1),2,2))
pi


#Coefficients
coef(hmm)



plot(hmm)

mpcNS <- viterbi.msm(hmmNS)
plot(mpcNS$time,mpcNS$observed,type="l")
lines(mpcNS$time,mpcNS$observed*(mpcNS$fitted-1),type="h",col=2)

stateprob <- function(hmm) {
  #Extract y|x GLM-coefs (on log scale) from HMM
  hmodel <- hmm$hmodel
  clabels1 <- hmodel$covlabels[hmodel$coveffstate == 1]
  clabels2 <- hmodel$covlabels[hmodel$coveffstate == 2]
  s1 <- c(log(hmodel$pars[1]),hmodel$coveffect[hmodel$coveffstate == 1])
  s2 <- c(log(hmodel$pars[2]),hmodel$coveffect[hmodel$coveffstate == 2])
  names(s1) <- c("intercept",clabels1)
  names(s2) <- c("intercept",clabels2)

  #Compute stationary distribution from pmatrix
  P <- pmatrix.msm(hmmNS,t=1)
  pi <-matrix(c(1,1),1,2) %*% solve(diag(c(1,1))-P + matrix(c(1,1,1,1),2,2))
  pi
  pxprio <- pi

  #allocate matrices
  pxpost <- matrix(NA,ncol=nrow(polio),nrow=2)
  rate <- matrix(NA,ncol=nrow(polio),nrow=2)

  #Forward pass
  for (i in 1:nrow(polio)) {
    rate[,i] <- exp(c(sum(cbind(1,hmm$data$covmat[i,clabels1]) * s1),
                      sum(cbind(1,hmm$data$covmat[i,clabels2]) * s2)))

    pyGx <- dpois(polio$y[i],lambda=rate[,i])
    py <- sum(pyGx * pxprio)

    pxpost[,i] <- pyGx * pxprio / py
    pxprio <- as.numeric(P %*% pxpost[,i] )
  }

  #Backward pass
  for (j in nrow(polio):1) {
    #missing
  }
  

  
  return(list(rate=rate,pxpost=pxpost))
}

x <- stateprob(hmmSt)
matplot(t(x$rate),type="l")
plot(x$pxpost[1,],type="l",main="State: Normal")
plot(x$pxpost[2,],type="l",main="State: Outbreak")
matplot(t(x$pxpost),type="l")
  
###Package integration
source("algo_hmm.R")

library(surveillance)
set.seed(123)
#Simulate outbreak data from HMM
counts <- sim.pointSource(p = 0.99, r = 0.8, length = 52*8,
                              A = 1, alpha = 1, beta = 0, phi = 0,
                              frequency = 1, state = NULL, K = 1.5)

#Do surveillance using a two state HMM without trend component and
#the effect of the harmonics being the same in both states
survRes <- algo.hmm(counts, control=list(range=1:length(counts$observed),noStates=2,trend=FALSE,covEffectsEqual=TRUE))
plot(survRes)

#Look at the estimated HMM (see ?msm for details)
survRes$control$hmm

#Extract transition matrix of HMM (see ?pmatrix.msm for details)
pmatrix.msm(survRes$control$hmm)

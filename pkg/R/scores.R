################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Scoring rules as discussed in:
### Predictive model assessment for count data
### Czado, C., Gneiting, T. & Held, L. (2009)
### Biometrics 65:1254-1261
###
### Copyright (C) 2010-2013 Michaela Paul and Sebastian Meyer
### $Revision$
### $Date$
################################################################################


## logarithmic score
# logs(P,x) = - log(P(X=x))
logScore <- function(x,mu, size=NULL){
	if(is.null(size))
		- dpois(x,lambda=mu,log=TRUE)
	else - dnbinom(x, mu=mu,size=size,log=TRUE)
}

## squared error score
# ses(P,x) =(x-mu_p)^2
ses <- function(x, mu){
  (x-mu)^2
}

## normalized squared error score
# nses(P,x) =((x-mu_p)/sigma_p)^2
nses <- function(x, mu,size=NULL){
  if(!is.null(size)){
    sigma2 <- mu*(1+mu/size)
  } else sigma2 <- mu
  
  ((x-mu)^2)/sigma2
}

## Dawid-Sebastiani score
# dss(P,x) = ((x-mu_p)/sigma_p)^2 + 2*log(sigma_p)
dss <- function(x,mu,size=NULL){
  if(!is.null(size)){
    sigma2 <- mu*(1+mu/size)
  } else sigma2 <- mu
  
  ((x-mu)^2)/sigma2 +log(sigma2)
}

## ranked probability score
# rps(P,x) =sum_0^Kmax { P(X<=k) - 1(x <=k)}^2
rps.one <- function(x, mu,size=NULL,k=40,eps=1e-10){
	# determine variance of distribution
	if(is.null(size)){
	  se <- sqrt(mu)
	} else se <- sqrt(mu*(1+mu/size))
	
	# determine the maximum number of summands as Kmax= mean+k*se
	kmax <- ceiling(mu + k*se)
	
	# compute 1(x <=k)
	ind <- 1*(x < (1:(kmax+1)))
	
	# compute P(X<=k)
	# Poisson case
	if(is.null(size)){
		Px <- ppois(0:kmax,lambda=mu)
	} else  Px <- pnbinom(0:kmax,mu=mu,size=size)
	
	#determine precision
	if((1-tail(Px,1))^2 > eps)
	 cat("precision of finite sum not smaller than ", eps,"\n")
	
	
	# compute rps
	sum((Px-ind)^2)
}

rps <- function(x,mu,size=NULL,k=40){
	n <- length(x)
	if(length(mu)==1)
		mu <- rep(mu,n)
	if(!is.null(size) & length(size)==1)
		size <- rep(size,n)
	
	res <- sapply(1:n, function(i) rps.one(x=x[i],mu=mu[i],size=size[i],k=k) )
	matrix(res,ncol=ncol(as.matrix(x)),byrow=FALSE)
}


## returns logs, rps,ses and dss in reversed!! order
## i.e. the scores for time points n, n-1, n-2,...
scores <- function(object, unit=NULL,sign=FALSE, individual=FALSE)
{
    mu <- object$pred
    x <- object$observed
    size <- object$psi

    if (!is.null(size)) { # NegBin
        size <- exp(size) # transform to parameterization suitable for dnbinom()
        if (ncol(size) != ncol(x)) { # => ncol(size)=1, unit-independent psi
            ## replicate to obtain nrow(size) x nUnits matrix
            size <- matrix(size, nrow=nrow(size), ncol=ncol(x), byrow=FALSE)
        }
    }
    
    if(!is.null(unit)){
        x <- as.matrix(x[,unit])
        mu <- as.matrix(mu[,unit])
        size <- size[,unit]
    }
    
    signXmMu <- if(sign) sign(x-mu) else NULL

    #compute average scores for unit
    log.score <- apply(as.matrix(logScore(x=x,mu=mu,size=size)),MARGIN=2,rev)
    rp.score <- apply(as.matrix(rps(x=x,mu=mu,size=size)),MARGIN=2,rev)
    se.score <- apply(as.matrix(ses(x=x,mu=mu)), MARGIN=2, rev)
    nse.score <- apply(as.matrix(nses(x=x,mu=mu,size=size)),MARGIN=2,rev)
    ds.score <- apply(as.matrix(dss(x=x, mu=mu, size=size)), MARGIN=2,rev)
    
    if(is.null(unit)){
        if(individual){
            log.score <- c(log.score)
            rp.score <- c(rp.score)
            se.score <- c(se.score)
            nse.score <- c(nse.score)
            ds.score <- c(ds.score)
        } else {
            log.score <- rowMeans(log.score)
            rp.score <- rowMeans(rp.score)
            se.score <- rowMeans(se.score)
            nse.score <- rowMeans(nse.score)
            ds.score <- rowMeans(ds.score)
        }    
    }
    
    result <- cbind(logs=log.score,rps=rp.score,ses=se.score,dss=ds.score,nses=nse.score,signXmMu=signXmMu)
    return(result)
}


#####################################################
## non-randomized version of the PIT histogram
##
## Params: 
## x - observed data
## pdistr - predictive CDF, i.e. a vectorized function (x, ...)
## J - number of bins 
## ... - arguments for pdistr
## plot - logical indicating if a plot should be produced, or a list of
##        arguments for plot.histogram
####################################################

pit <- function (x, pdistr, J=10, relative=TRUE, ..., plot = FALSE)
{
    stopifnot(pdistr(-1, ...) == 0)
    breaks <- (0:J)/J
    Fbar_seq <- sapply(breaks, pit.one, x=x, pdistr=pdistr, ...)
    scale <- if (relative) J else 1
    f_j <- scale * diff(Fbar_seq)
    
    res <- structure(list(breaks=breaks, counts=f_j, density=f_j,
                          mids=breaks[-(J+1)] + 1/J/2,
                          xname="PIT", equidist=TRUE),
                     class="histogram")

    ## plot
    if (isTRUE(plot)) plot <- list()
    if (is.list(plot)) {
        defaultArgs <- list(
            x = res,
            ylab = if (relative) "Relative frequency" else "Density",
            main = ""
            )
        plot[["x"]] <- NULL             # manual x is ignored
        plot.args <- modifyList(defaultArgs, plot)
        do.call("plot", plot.args)
        invisible(res)
    } else res
}

## calculate \bar{F}(u) for scalar u
pit.one <- function (u, x, pdistr, ...)
{
    if (u <= 0) return(0) else if (u >= 1) return(1)
    Px <- pdistr(x, ...)
    Pxm1 <- pdistr(x-1, ...)             # ifelse(x==0, 0, pdistr(x-1, ...))
    F_u <- (u-Pxm1) / (Px-Pxm1)
    ## If Px=Pxm1, this means that predict. prob. of observed x is exactly zero.
    ## We get NaN for F_u. Our predictive model is bad if that happens.
    ## We could assign either 0 or 1 to express that and issue a warning.
    if (any(is.nan(F_u))) {
        warning("predictive distribution has 0 probability for observed 'x'")
        F_u[is.nan(F_u)] <- 0
    }
    F_u[F_u < 0] <- 0
    F_u[F_u > 1] <- 1
    mean(F_u)
}



#######################################################
## scores1, scores2 - vector with scores from two models
#########################################################
permutationTest <- function(score1,score2, nPermutation=9999,plot=FALSE){
  meanScore1 <- mean(score1)
  meanScore2 <- mean(score2)
  diffObserved <- meanScore1 - meanScore2
  
  nTime <- length(score1)
  diffMean <- replicate(nPermutation, {
      sel <- rbinom(nTime, size=1, prob=0.5)
      g1 <- (sum(score1[sel==0]) + sum(score2[sel==1]))/nTime
      g2 <- (sum(score1[sel==1]) + sum(score2[sel==0]))/nTime
      g1 - g2
  })

  if(plot){
    hist(diffMean, nclass=50, prob=TRUE,xlab="Difference between means",main="")
    abline(v=diffObserved,col=4)
  }
  
  pVal <- (1+sum(abs(diffMean)>=abs(diffObserved)))/(nPermutation+1)

  pTtest <- t.test(score1,score2,paired=TRUE)$p.value
  
  cat("mean difference=",diffObserved,"\tp(permutation) =",pVal,"\tp(paired t-test) =",pTtest,"\n")
  return(list(diffObs=diffObserved, pVal.permut=pVal,pVal.t=pTtest))
}


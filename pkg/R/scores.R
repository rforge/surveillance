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
### Copyright (C) 2010-2012 Michaela Paul, 2014 Sebastian Meyer
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


## returns logs, rps, ses, dss, nses in reversed (!) order
## i.e. the scores for time points n, n-1, n-2, ...
scores <- function (object, units=NULL, sign=FALSE, individual=FALSE)
{
    mu <- object$pred     # predicted counts
    x <- object$observed  # observed counts
    size <- object$psi    # estimated -log(overdispersion), 1 or ncol(x) columns
    ntps <- nrow(x)       # the number of predicted time points
    
    if (!is.null(size)) { # => NegBin
        size <- exp(size) # transform to parameterization suitable for dnbinom()
        if (ncol(size) != ncol(x)) { # => ncol(size)=1, unit-independent psi
            ## replicate to obtain a ntps x ncol(x) matrix
            size <- matrix(size, nrow=ntps, ncol=ncol(x), byrow=FALSE)
        }
        colnames(size) <- colnames(x)  # such that we can select by unit name
    }
    ## At this point, mu, x and size all are ntps x ncol(x) matrices

    ## select units
    if (!is.null(units)) {
        x <- x[,units,drop=FALSE]
        mu <- mu[,units,drop=FALSE]
        size <- size[,units,drop=FALSE]
    }
    nUnits <- ncol(x)
    if (nUnits == 1L)
        individual <- TRUE  # no need to apply rowMeans() below

    ## compute sign of x-mu
    signXmMu <- if(sign) sign(x-mu) else NULL
    
    ## compute individual scores
    log.score <- logScore(x=x, mu=mu, size=size)
    rp.score <- rps(x=x, mu=mu, size=size)
    se.score <- ses(x=x, mu=mu)
    ds.score <- dss(x=x, mu=mu, size=size)
    nse.score <- nses(x=x, mu=mu, size=size)
    
    ## gather individual scores in an array
    result <- array(c(log.score, rp.score, se.score, ds.score, nse.score, signXmMu),
                    dim = c(ntps, nUnits, 5L + sign),
                    dimnames = c(dimnames(x),
                                 list(c("logs","rps","ses", "dss", "nses",
                                        if (sign) "sign"))))

    ## reverse order of the time points (historically)
    result <- result[ntps:1L,,,drop=FALSE]

    ## average over units if requested
    if (individual) {
        drop(result)
    } else {
        apply(X=result, MARGIN=3L, FUN=rowMeans)
        ## this gives a ntps x (5L+sign) matrix (or a vector in case ntps=1)
    }
}

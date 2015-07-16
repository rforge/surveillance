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
### Copyright (C) 2010-2012 Michaela Paul, 2014-2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


## logarithmic score
## logs(P,x) = -log(P(X=x))

logs <- function (x, mu, size=NULL) {
    if (is.null(size)) {
        - dpois(x, lambda=mu, log=TRUE)
    } else {
        - dnbinom(x, mu=mu, size=size, log=TRUE)
    }
}


## squared error score
## ses(P,x) = (x-mu_p)^2

ses <- function (x, mu, size=NULL) {
    (x-mu)^2
}


## normalized squared error score (IMPROPER)
## nses(P,x) = ((x-mu_p)/sigma_p)^2

nses <- function (x, mu, size=NULL) {
    sigma2 <- if (is.null(size)) mu else mu * (1 + mu/size)
    ((x-mu)^2) / sigma2
}


## Dawid-Sebastiani score
## dss(P,x) = ((x-mu_p)/sigma_p)^2 + 2*log(sigma_p)

dss <- function (x, mu, size=NULL) {
    sigma2 <- if (is.null(size)) mu else mu * (1 + mu/size)
    ((x-mu)^2)/sigma2 + log(sigma2)
}


## ranked probability score
## rps(P,x) = sum_0^Kmax {P(X<=k) - 1(x<=k)}^2

## scalar input (for one single prediction)
rps.one <- function (x, mu, size=NULL, k=40, tolerance=sqrt(.Machine$double.eps)) {
    ## return NA for non-convergent fits (where mu=NA)
    if (is.na(mu)) return(NA_real_)

    ## determine variance of distribution
    sigma2 <- if (is.null(size)) mu else mu * (1 + mu/size)
    
    ## determine the maximum number of summands as Kmax=mean+k*sd
    kmax <- ceiling(mu + k*sqrt(sigma2))
    
    ## compute 1(x<=k)
    ind <- 1*(x < seq_len(kmax+1))
	
    ## compute P(X<=k)
    Px <- if (is.null(size)) {
        ppois(0:kmax, lambda=mu)
    } else {
        pnbinom(0:kmax, mu=mu, size=size)
    }
	
    ## check precision
    if ((1-tail(Px,1))^2 > tolerance)
        warning("finite sum approximation error larger than tolerance=",
                format(tolerance))
	
    ## compute rps
    sum((Px-ind)^2)
}

## vectorized version
rps <- function (x, mu, size=NULL, k=40, tolerance=sqrt(.Machine$double.eps)) {
    res <- if (is.null(size)) {
        mapply(rps.one, x=x, mu=mu,
               MoreArgs=list(k=k, tolerance=tolerance), SIMPLIFY=TRUE, USE.NAMES=FALSE)
    } else {
        mapply(rps.one, x=x, mu=mu, size=size,
               MoreArgs=list(k=k, tolerance=tolerance), SIMPLIFY=TRUE, USE.NAMES=FALSE)
    }
    attributes(res) <- attributes(x)  # set dim and dimnames
    res
}


### apply a set of scoring rules at once

scores.default <- function(x, mu, size,
                           which = c("logs", "rps", "dss", "ses"),
                           sign = FALSE, ...)
{
    ## compute sign of x-mu
    signXmMu <- if (sign) sign(x-mu) else NULL
    
    ## compute individual scores (these are dim(x) matrices)
    scorelist <- lapply(which, do.call, args = alist(x=x, mu=mu, size=size),
                        envir = environment())
    
    ## gather individual scores in an array
    array(c(unlist(scorelist, recursive=FALSE, use.names=FALSE),
            signXmMu),
          dim = c(dim(x), length(which) + sign),
          dimnames = c(dimnames(x),
              list(c(which, if (sign) "sign"))))
}

### apply scoring rules to a set of oneStepAhead() forecasts
## CAVE: returns scores in reversed order, i.e. for time points n, n-1, n-2, ...

scores.oneStepAhead <- function (x, which = c("logs","rps","dss","ses"),
                                 units = NULL, sign = FALSE, individual = FALSE,
                                 reverse = TRUE, ...)
{
    y <- x$observed  # observed counts during the prediction window
    mu <- x$pred     # predicted counts (same dim as y)
    ## transform overdispersion to dnbinom() parameterization
    size <- psi2size.oneStepAhead(x) # -> NULL or full dim(y) matrix

    ## select units
    if (!is.null(units)) {
        y <- y[,units,drop=FALSE]
        mu <- mu[,units,drop=FALSE]
        size <- size[,units,drop=FALSE] # works with size = NULL
    }
    nUnits <- ncol(y)
    if (nUnits == 1L)
        individual <- TRUE  # no need to apply rowMeans() below

    result <- scores.default(x = y, mu = mu, size = size,
                             which = which, sign = sign)
    
    ## reverse order of the time points (historically)
    if (reverse)
        result <- result[nrow(result):1L,,,drop=FALSE]

    ## average over units if requested
    if (individual) {
        drop(result)
    } else {
        apply(X=result, MARGIN=3L, FUN=rowMeans)
        ## this gives a nrow(y) x (5L+sign) matrix (or a vector in case nrow(y)=1)
    }
}


## calculate scores with respect to fitted values

scores.hhh4 <- function (x, which = c("logs","rps","dss","ses"),
                         subset = x$control$subset, units = seq_len(x$nUnit),
                         sign = FALSE, ...)
{
    ## slow implementation via "fake" oneStepAhead():
    ##fitted <- oneStepAhead(x, tp = subset[1L] - 1L, type = "final",
    ##                       keep.estimates = FALSE, verbose = FALSE)
    ##scores.oneStepAhead(fitted, which = which, units = units, sign = sign,
    ##                    individual = TRUE, reverse = FALSE)

    result <- scores.default(
        x = x$stsObj@observed[subset, units, drop = FALSE],
        mu = x$fitted.values[match(subset, x$control$subset), units, drop = FALSE],
        size = psi2size.hhh4(x, subset, units),
        which = which, sign = sign)
    drop(result)
}

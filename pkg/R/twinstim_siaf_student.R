################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Student (t) kernel f(s) = (||s||^2+sigma^2)^-d
### This is a reparametrization of the t-kernel; For d=1, this is the kernel of
### the Cauchy density with scale sigma; in Geostatistics, a correlation
### function of this kind is known as the Cauchy model.
###
### Copyright (C) 2013 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


siaf.student <- function (nTypes = 1, validpars = NULL)
{
    nTypes <- as.integer(nTypes)
    stopifnot(length(nTypes) == 1L, nTypes > 0L)

    ## for the moment we don't make this type-specific
    if (nTypes != 1) stop("type-specific shapes are not yet implemented")

    ## helper expression, note: logpars=c(logscale=logsigma, logd=logd)
    tmp <- expression(
        logsigma <- logpars[[1L]],  # used "[[" to drop names
        logd <- logpars[[2L]],
        sigma <- exp(logsigma),
        d <- exp(logd)
        )

    ## spatial kernel
    f <- function (s, logpars, types = NULL) {}
    body(f) <- as.call(c(as.name("{"),
        tmp,
        expression(s2 <- .rowSums(s^2, nrow(s), 2L)),
        expression((s2+sigma^2)^-d)
    ))

    ## numerically integrate f over a polygonal domain
    F <- function (polydomain, f, logpars, type = NULL, ...)
        .polyCub.iso(polydomain$bdry, intrfr.student,
                     exp(logpars[[1L]]), exp(logpars[[2L]]),
                     center=c(0,0), control=list(...))
    
    ## fast integration of f over a circular domain
    ## is not relevant for this heavy-tail kernel since we don't use
    ## effRange, and usually eps.s=Inf
    ##Fcircle <- function (r, logpars, type = NULL) {}

    ## derivative of f wrt logpars
    deriv <- function (s, logpars, types = NULL) {}
    body(deriv) <- as.call(c(as.name("{"),
        tmp,
        expression(
            s2 <- .rowSums(s^2, nrow(s), 2L),
            s2sigma2d <- (s2+sigma^2)^d,
            derivlogsigma <- -2*d*sigma^2 / s2sigma2d / (s2+sigma^2),
            derivlogd <- -log(s2sigma2d) / s2sigma2d,
            cbind(derivlogsigma, derivlogd)
            )
    ))

    ## Numerical integration of 'deriv' over a polygonal domain
    Deriv <- function (polydomain, deriv, logpars, type = NULL, ...) {}
    body(Deriv) <- as.call(c(as.name("{"),
        tmp,
        expression(
            res.logsigma <- .polyCub.iso(polydomain$bdry,
                                         intrfr.student.dlogsigma,
                                         sigma, d, center=c(0,0),
                                         control=list(...)),
            res.logd <- .polyCub.iso(polydomain$bdry,
                                     intrfr.student.dlogd,
                                     sigma, d, center=c(0,0),
                                     control=list(...)),
            c(res.logsigma, res.logd)
            )
    ))

    ## simulate from the power-law kernel (within a maximum distance 'ub')
    ## Sampling from f_{2D}(s) \propto f(||s||), truncated to ||s|| <= ub,
    ## works via polar coordinates and the inverse transformation method:
    ## p_{2D}(r,theta) = r * f_{2D}(x,y) \propto r*f(r)
    ## => sample angle theta from U(0,2*pi) and r according to r*f(r)
    simulate <- function (n, logpars, type, ub)
    {
        ## Note: in simEpidataCS, simulation is always bounded to eps.s and to
        ## the largest extend of W, thus, 'ub' is finite
        stopifnot(is.finite(ub))
        sigma <- exp(logpars[[1L]])
        d <- exp(logpars[[2L]])

        ## Normalizing constant of r*f(r) on [0;ub]
        normconst <- intrfr.student(ub, sigma, d)
        ## normconst <- if (is.finite(ub)) intrfr.student(ub, sigma, d) else {
        ##     ## for sampling on [0;Inf] the density is only proper if d > 1
        ##     if (d <= 1) stop("improper density for d<=1, 'ub' must be finite")
        ##     sigma^(2-2*d) / (2*d-2) # = intrfr.student(Inf, sigma, d)
        ## }

        ## => cumulative distribution function
        CDF <- function (q) intrfr.student(q, sigma, d) / normconst

        ## For inversion sampling, we need the quantile function CDF^-1
        ## However, this is not available in closed form, so we use uniroot,
        ## which requires a finite upper bound!
        QF <- function (p) uniroot(function(q) CDF(q)-p, lower=0, upper=ub)$root

        ## Now sample r as QF(U), where U ~ U(0,1)
        r <- sapply(runif(n), QF)
        ## Check simulation via kernel estimate:
        ## plot(density(r, from=0, to=ub)); curve(p(x) / normconst, add=TRUE, col=2)
        
        ## now rotate each point by a random angle to cover all directions
        theta <- stats::runif(n, 0, 2*pi)
        r * cbind(cos(theta), sin(theta))
    }

    ## set function environments to the global environment
    environment(f) <-  environment(deriv) <- .GlobalEnv
    ## in F, Deriv, and simulate we need access to the intrfr-functions
    environment(F) <- environment(Deriv) <- environment(simulate) <-
        getNamespace("surveillance")

    ## return the kernel specification
    list(f=f, F=F, deriv=deriv, Deriv=Deriv, simulate=simulate,
         npars=2L, validpars=validpars)
}


## integrate x*f(x) from 0 to R (vectorized)
intrfr.student <- function (R, sigma, d)
{
    if (d == 1) {
        log(R^2+sigma^2) / 2 - log(sigma)
    } else {
        ( (R^2+sigma^2)^(-d+1) - (sigma^2)^(-d+1) ) / (2-2*d)
    }
}

## integrate x * (df(x)/dlogsigma) from 0 to R (vectorized)
intrfr.student.dlogsigma <- function (R, sigma, d)
    sigma^2 * ( (R^2+sigma^2)^-d - sigma^(-2*d) )

## integrate x * (df(x)/dlogd) from 0 to R (vectorized)
intrfr.student.dlogd <- function (R, sigma, d)
{
    if (d == 1) {
        log(sigma)^2 - log(R^2+sigma^2)^2 / 4
    } else { # thanks to Maple 17
        primitive <- function (x) {
            x2ps2 <- x^2 + sigma^2
            (d*(d-1)*log(x2ps2) + d) / (2*(d-1)^2 * (x2ps2)^(d-1))
        }
        primitive(R) - primitive(0)
    }
}

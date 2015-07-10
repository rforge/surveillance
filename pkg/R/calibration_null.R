################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Expectation and variance of proper scoring rules for Poisson and NegBin
### Reference: Wei and Held (2014), Test, 23, 787-805
###
### Copyright (C) 2013-2014 Wei Wei, 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

## NOTE: size = NULL refers to Poisson predictions, otherwise NegBin

##########################
### Dawid–Sebastiani Score
##########################

dss_EV <- function (mu, size = NULL, tolerance = NULL)
{
    sigma2 <- if (is.null(size)) mu else mu * (1 + mu/size)
    E <- 1 + log(sigma2)
    V <- if (is.null(size)) {
        2 + 1/sigma2
    } else {
        2 + 6/size + 1/sigma2
    }
    list(E = E, V = V)
}


#####################
### Logarithmic Score
#####################

logs_EV <- function (mu, size = NULL, tolerance = 1e-4)
{
    res <- if (is.null(size)) {
        vapply(X = mu, FUN = logs_EV_1P, tolerance = tolerance,
               FUN.VALUE = c(E = 0, V = 0), USE.NAMES = FALSE)
    } else {
        mapply(FUN = logs_EV_1NB, mu = mu, size = size,
               MoreArgs = list(tolerance = tolerance),
               SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }
    ## 'res' has dimension 2 x length(mu)
    list(E = res[1L,], V = res[2L,])
}

## for a single Poisson prediction
logs_EV_1P <- function (mu, tolerance = 1e-4) # tolerance is in absolute value
{
    ## use the same kmax for expectation and variance -> shared computations
    ## K2 is always a bit larger than K1, so we use K2
    kmax <- if (mu^3 < tolerance/.Machine$double.eps/2) {
        ## we can calculate K2 from Theorem 1 (b)
        qpois(1 - tolerance/(mu^3 + 6*mu^2 + 7*mu + 1), lambda = mu) + 3
    } else { # very high quantile (e.g., 1 - 1e-16) would yield Inf
        mu + 10 * sqrt(mu)
    }
    kseq <- seq_len(kmax)

    ## compute values required by both E and V
    fseq <- dpois(kseq, lambda = mu)
    logfactseq <- lfactorial(kseq)
    
    ## expectation
    E <- if (mu > tolerance^(-1/4)) { # fast version for "large" mu
        ## approximation error is of order 1/mu^4
        0.5 + 0.5*log(2*pi*mu) - 1/12/mu - 1/24/mu^2 - 19/360/mu^3
    } else {
        ##kmax1 <- qpois(1 - tolerance/(mu^2 + 3*mu + 1), lambda = mu) + 2
        seqq1 <- fseq * logfactseq
        mu * (1-log(mu)) + sum(seqq1)
    }
    
    ## variance (does it converge to 0.5 as mu -> Inf ?)
    seqq2 <- (logfactseq - kseq * log(mu))^2 * fseq
    V <- sum(seqq2) - (E - mu)^2
    
    c(E = E, V = V)
}

## for a single NegBin prediction
logs_EV_1NB <- function (mu, size, tolerance = 1e-4)
{
    ## TODO: replace simple kmax by formulae from the paper
    kmax <- qnbinom(1-tolerance/10, mu = mu, size = size) + 5
    kseq <- 0:kmax

    ## compute values required by both E and V
    fseq <- dnbinom(kseq, mu = mu, size = size)
    lgammaseq <- lbeta(kseq + 1L, size) + log(kseq + size)
    
    ## expectation
    seqq1 <- lgammaseq * fseq
    E <- sum(seqq1) - size*log(size) - mu*log(mu) + (mu+size)*log(mu+size)

    ## variance
    con2 <- E - size * log(1 + mu/size)
    seqq2 <- (lgammaseq + kseq * log(1 + size/mu))^2 * fseq
    V <- sum(seqq2) - con2^2
    ## check against formulation in the paper (Equation 11):
    ## con2paper <- E + size*log(size) - size*log(size+mu) - lgamma(size)
    ## seqq2paper <- (-lgamma(kseq+size) + lgamma(kseq+1L) + kseq*log(1+size/mu))^2 * fseq
    ## Vpaper <- sum(seqq2paper) - con2paper^2
    ## => V and Vpaper are only identical for kmax -> Inf
    
    c(E = E, V = V)
}


############################
### Ranked Probability Score
############################

rps_EV <- function (mu, size = NULL, tolerance = 1e-4)
{
    .NotYetImplemented()
}

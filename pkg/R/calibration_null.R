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

dss_EV <- function (mu, size = NULL)
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

logs_EV <- function (mu, size = NULL)
{
    res <- if (is.null(size)) {
        vapply(X = mu, FUN = logs_EV_1P,
               FUN.VALUE = c(E = 0, V = 0), USE.NAMES = FALSE)
    } else {
        mapply(logs_EV_1NB, mu = mu, size = size,
               SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }
    ## 'res' has dimension 2 x length(mu)
    list(E = res[1L,], V = res[2L,])
}

## for a single Poisson prediction
logs_EV_1P <- function (mu)
{
    kmax <- qpois(1-10^(-5), lambda = mu) + 5
    E <- if (mu > 10) {
        0.5 + 0.5*log(2*pi*mu) - 1/12/mu - 1/24/mu/mu - 19/360/mu/mu/mu
    } else {
        seqq <- c(0, sapply(1:kmax, function(i) {
            r <- i*log(mu)+log(sum(log(seq_len(i))))-sum(log(seq_len(i)))-mu
            exp(r)
            ## = mu^i * sum(log(seq_len(i))) / prod(seq_len(i)) / exp(mu)
        }))
        mu*(1-log(mu)) + sum(seqq)
    }
    ##variance
    seqq.m <- sapply(0:kmax, function(i) {
        if (i==0) {
            r2 <- 2*log(mu) - mu
        } else {
            r <- (sum(log(1:i))-log(mu)*i+mu)^2 
            r2 <- log(r)+i*log(mu)-sum(log(1:i))-mu
        }
        exp(r2)
    })
    V <- sum(seqq.m) - E^2
    c(E = E, V = V)
}

## for a single NegBin prediction
logs_EV_1NB <- function (mu, size)
{
    kmax <- qnbinom(1-10^(-5), mu = mu, size = size) + 5
    seqq <- sapply(0:kmax, function(i)
        (lbeta(i+1,size) + log(i+size)) *
            dnbinom(i, mu=mu, size=size))
    E <- sum(seqq) - size*log(size) - mu*log(mu) + (mu+size)*log(mu+size)
    ##variance
    con2 <- E - size*log((mu+size)/size)
    seqq2 <- sapply(0:kmax, function(i)
        ((lbeta(i+1,size) + log(i+size)) + i*log(1+size/mu))^2 *
            dnbinom(i, mu=mu, size=size))
    V <- sum(seqq2) - con2^2
    c(E = E, V = V)
}


############################
### Ranked Probability Score
############################

rps_EV <- function (mu, size = NULL)
{
    .NotYetImplemented()
}

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


### Dawid–Sebastiani Score

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


### Logarithmic Score

logs_EV <- function (mu, size = NULL)
{
    .NotYetImplemented()
}


### Ranked Probability Score

rps_EV <- function (mu, size = NULL)
{
    .NotYetImplemented()
}

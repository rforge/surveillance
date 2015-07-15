################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### calibrationTest() for "hhh4" fits
###
### Copyright (C) 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

calibrationTest.hhh4 <- function (x,
                                  subset = x$control$subset,
                                  units = seq_len(x$nUnit),
                                  ...)
{
    ## extract estimated overdispersion
    pars <- splitParams(x$coefficients, terms.hhh4(x))
    size <- exp(pars$overdisp) # transform to parametrization of dnbinom()
    if (length(size) > 1L) { # => length(size) == x$nUnit
        names(size) <- colnames(x$stsObj) # to select 'units'
        size <- matrix(size[units], length(subset), length(units), byrow = TRUE)
    }

    ## perform the calibration test in the specified subset
    res <- calibrationTest.default(
        x = x$stsObj@observed[subset, units, drop = FALSE],
        mu = x$fitted.values[match(subset, x$control$subset), units, drop = FALSE],
        size = if (length(size)) size else NULL,
        ...)

    ## change "data.name" to be the name of the supplied model
    res$data.name <- deparse(substitute(x))
    res
}

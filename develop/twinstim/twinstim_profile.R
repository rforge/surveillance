################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Profile Likelihood Evaluation for "twinstim" Models (using update.twinstim)
###
### Copyright (C) 2014 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

twinstimProfile <- function (fitted, which=1, grid=10)
{
    stopifnot(length(which) == 1)
    grid <- if (isScalar(grid)) { # use equidistant evaluation points within Wald-CI
        waldci <- confint(fitted, parm=which, level=0.95)[1,]
        seq(waldci[1L], waldci[2L], length.out=grid)
    } else {
        stopifnot(is.vector(grid, mode="numeric"))
        sort(grid)
    }
    ngrid <- length(grid)

    ## loop over the grid
    start <- coef(fitted)
    coefs <- matrix(NA_real_, ngrid, length(start),
                    dimnames=list(NULL, names(start)))
    logliks <- numeric(ngrid)
    for (i in seq_len(ngrid)) {
        cat("fitting the model at grid point", i, "/", ngrid, "...\n")
        start[which] <- grid[i]
        fit <- update(fitted, optim.args=list(par=start, fixed=which),
                      model=FALSE, cumCIF=FALSE, verbose=FALSE)
        coefs[i,] <- fit$coefficients
        logliks[i] <- fit$loglik
    }

    ## add original MLE (if not already part of the grid)
    mle <- coef(fitted)
    if (!mle[which] %in% grid) {
        coefs <- rbind(coefs, mle, deparse.level=0)
        logliks <- c(logliks, fitted$loglik)
        ord <- order(coefs[,which])
        coefs <- coefs[ord,]
        logliks <- logliks[ord]
    }

    ## done
    res <- cbind(coefs = coefs, loglik = logliks)
    attr(res, "which") <- which
    res
}

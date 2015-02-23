################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Knox test for space-time interaction
###
### Copyright (C) 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


knox <- function (dt, ds, eps.t, eps.s, simulate.p.value = FALSE, B = 999, ...)
{
    stopifnot(length(dt) == length(ds))

    ## logical vectors indicating which pairs are close in time and space
    closeInTime <- if (is.logical(dt)) {
        dt
    } else {
        stopifnot(is.numeric(dt), isScalar(eps.t))
        dt <= eps.t
    }
    closeInSpace <- if (is.logical(ds)) {
        ds
    } else {
        stopifnot(is.numeric(ds), isScalar(eps.s))
        ds <= eps.s
    }

    ## manually build the contingency table (table() with factor() is too slow)
    .lab <- c("close", "not close")
    knoxtab <- array(
        tabulate(4L - closeInTime - 2L*closeInSpace, nbins = 4L),
        dim = c(2L, 2L),
        dimnames = list(
            dt = if (is.logical(dt)) .lab else paste(c("<=", " >"), eps.t),
            ds = if (is.logical(ds)) .lab else paste(c("<=", " >"), eps.s)
        ))
    class(knoxtab) <- "table"

    ## expected number of close pairs in the absence of spatio-temporal interaction
    npairs <- sum(knoxtab)
    expected <- sum(knoxtab[1L,]) / npairs * sum(knoxtab[,1L])
    ##<- this order of terms avoids integer overflow
    
    ## test statistic is the number of spatio-temporally close pairs
    METHOD <- "Knox test"
    STATISTIC <- knoxtab[1L]

    ## determine statistical significance
    pval_Poisson <- ppois(STATISTIC, expected, lower.tail = FALSE)
    PVAL <- if (simulate.p.value) { # Monte Carlo permutation approach
        stopifnot(isScalar(B))
        B <- as.integer(B)
        METHOD <- paste(METHOD, "with simulated p-value\n\t (based on", 
                        B, "replicates)")
        permstats <- plapply(X = integer(B), FUN = function (...)
            sum(closeInSpace & closeInTime[sample.int(npairs)]), ...)
        ## boxplot(unlist(permstats), ylim = range(STATISTIC, permstats))
        ## points(STATISTIC, pch = 4, lwd = 2)
        structure(mean(c(STATISTIC, permstats, recursive = TRUE) >= STATISTIC),
                  Poisson = pval_Poisson)
    } else {
        METHOD <- paste(METHOD, "with Poisson approximation")
        pval_Poisson
    }

    ## return test results
    structure(
        list(method = METHOD,
             data.name = paste("dt =", deparse(substitute(dt)),
                               "and ds =", deparse(substitute(ds))),
             statistic = setNames(STATISTIC, "number of close pairs"),
             p.value = PVAL, alternative = "greater",
             null.value = setNames(expected, "number"),
             table = knoxtab),
        ##null.distribution = unlist(permstats, recursive=FALSE, use.names=FALSE)
        class = c("knox", "htest")
    )
}

print.knox <- function (x, ...)
{
    ## first print by the default method for class "htest"
    NextMethod("print")

    ## then also output the contingency table
    cat("contingency table:\n")
    print(x$table)
    cat("\n")
    invisible(x)
}

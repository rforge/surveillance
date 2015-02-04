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


knox <- function (dt, ds, eps.t, eps.s, simulate.p.value = FALSE, B = 999)
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
    
    ## test statistic is the number of spatio-temporally close pairs
    METHOD <- "Knox test"
    STATISTIC <- knoxtab[1L]
    npairs <- sum(knoxtab)
    PVAL <- if (simulate.p.value) { # Monte Carlo permutation approach
        stopifnot(isScalar(B))
        B <- as.integer(B)
        METHOD <- paste(METHOD, "with simulated p-value\n\t (based on", 
                        B, "replicates)")
        permstats <- vapply( # replicate() uses sapply()
            X = integer(B),
            FUN = function (...)
                sum(closeInSpace & closeInTime[sample.int(npairs)]),
            FUN.VALUE = 0L, USE.NAMES = FALSE)
        ## boxplot(permstats, ylim = range(STATISTIC, permstats)); points(STATISTIC, pch = 16)
        mean(c(STATISTIC, permstats) >= STATISTIC)
    } else {
        METHOD <- paste(METHOD, "with Poisson approximation")
        expected <- sum(knoxtab[1L,]) / npairs * sum(knoxtab[,1L])
        ppois(STATISTIC, expected, lower.tail = FALSE)
    }

    ## return test results
    structure(
        list(statistic = setNames(STATISTIC, "number of close pairs"),
             p.value = PVAL, method = METHOD,
             data.name = paste("dt =", deparse(substitute(dt)),
                               "and ds =", deparse(substitute(ds))),
             table = knoxtab), #null.distribution = permstats
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

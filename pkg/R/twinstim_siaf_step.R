################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### twinstim's spatial interaction function as a step function
###
### Copyright (C) 2014 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


siaf.step <- function (knots, maxRange = Inf, nTypes = 1, validpars = NULL)
{
    knots <- sort(unique(as.vector(knots,mode="numeric")))
    stopifnot(isScalar(maxRange), knots > 0, maxRange > knots)
    nknots <- length(knots)             # = number of parameters (per type)
    allknots <- unique(c(0,knots,maxRange,Inf))

    nTypes <- as.integer(nTypes)
    stopifnot(length(nTypes) == 1L, nTypes > 0L)
    ## for the moment we don't make this type-specific
    if (nTypes != 1) stop("type-specific shapes are not yet implemented")
    npars <- nknots * nTypes

    ## ## auxiliary function to get the type-specific values (heights) from logvals
    ## logvals4type <- function (logvals, type)
    ##     logvals[(type-1)*nknots + seq_len(nknots)]

    ## auxiliary function calculating the areas of the "rings" of the
    ## two-dimensional step function intersected with a polydomain
    .ringAreas <- function (polydomain, npoly = 256) {
        polyvertices <- vertices(polydomain)
        polyarea <- area.owin(polydomain)
        bdist <- bdist(cbind(0,0), polydomain)
        ## distance to farest vertex (-> later steps not relevant)
        R <- sqrt(max(polyvertices[["x"]]^2 + polyvertices[["y"]]^2))
        ## disable redundant checking when creating disc()s:
        oopt <- spatstat.options(checkpolygons=FALSE)
        on.exit(spatstat.options(oopt))
        sliceAreas <- sapply(allknots[-1L], function (r) {
            if (r <= bdist) pi * r^2 else if (r >= R) polyarea else
            area.owin(intersectPolyCircle.owin(polydomain,c(0,0),r,npoly=npoly))
        }, simplify=TRUE, USE.NAMES=FALSE)
        diff.default(c(0,sliceAreas))
    }
    ## since this is the most cumbersome task, use memoization (result does not
    ## depend on the parameters being optimized, but on influenceRegions only)
    ringAreas <- if (requireNamespace("memoise")) {
        memoise::memoise(.ringAreas)
    } else {
        warning("siaf.step() is much slower without memoisation",
                immediate.=TRUE)
        .ringAreas
    }

    f <- function (s, logvals, types = NULL) {
        sLength <- sqrt(.rowSums(s^2, length(s)/2, 2L))
        ## step function is right-continuous, intervals are [a,b)
        c(1, exp(logvals), 0)[.bincode(sLength, allknots, right=FALSE)]
    }

    F <- function (polydomain, f, logvals, type = NULL, npoly = 256) {
        ringAreas <- ringAreas(polydomain, npoly=npoly)
        sum(c(1, exp(logvals)) * ringAreas[seq_len(1L+npars)])
    }

    Fcircle <- function (r, logvals, type = NULL) { # exact integration on disc
        vals <- c(1, exp(logvals), 0)
        knots2r <- c(allknots[allknots < r], r)
        vals2r <- vals[seq_len(length(knots2r)-1L)]
        sum(vals2r * pi * diff(knots2r^2)) # sum of "ring" volumes
    }

    deriv <- function (s, logvals, types = NULL) {
        sLength <- sqrt(.rowSums(s^2, L <- length(s)/2, 2L))
        whichvals <- .bincode(sLength, allknots, right=FALSE) - 1L
        ## NOTE: sLength >= maxRange => whichvals > npars (=> f=0)
        ## we do a bare-bone implementation of relevant parts of
        ## deriv <- outer(whichvals, seq_len(npars), "==")
        Y <- rep.int(seq_len(npars), rep.int(L,npars)) # column index
        Z <- rep.int(exp(logvals), rep.int(L,npars))   # value
        ##<- 6x faster than rep(..., each=L)
        #X <- rep.int(whichvals, npars)
        deriv <- (Y == whichvals) * Z
        dim(deriv) <- c(L, npars)
        deriv
    }

    Deriv <- function (polydomain, deriv, logvals, type = NULL, npoly = 256) {
        ringAreas <- ringAreas(polydomain, npoly=npoly)
        exp(logvals) * ringAreas[1L+seq_len(npars)]
    }

    simulate <- function (n, logvals, type, ub) {
        .NotYetImplemented()
    }
    
    ## Done
    res <- list(f = f, F = F, Fcircle = Fcircle,
                deriv = deriv, Deriv = Deriv,
                simulate = simulate,
                npars = npars, validpars = validpars)
    attr(res, "knots") <- knots
    attr(res, "maxRange") <- maxRange
    res
}

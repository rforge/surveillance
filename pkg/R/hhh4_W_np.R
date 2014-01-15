################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Non-parametric specification of neighbourhood weights in hhh4()
###
### Copyright (C) 2014 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


### non-parametric estimation of weight function, i.e., provide each
### neighbourhood order up to 'maxlag' with its own (unconstrained) weight
### for identifiability:
### - first order is fixed to weight=1
### - usually maxlag < max(nborder) (since only few pairs with highest orders),
###   and to0 indicates which weight is assumed for orders > maxlag, either zero
###   or the same as for order 'maxlag'

W.np <- function (maxlag, to0 = FALSE, normalize = FALSE,
                  initial = log(zetaweights(2:maxlag)))
{
    if (normalize) stop("normalization is not implemented yet")
    if (missing(maxlag)) {
        stop("'maxlag' must be specified (usually < max. neighbourhood order)")
    } else stopifnot(isScalar(maxlag), maxlag > 1) # at least one parameter
    
    w <- function (loghoweights, nbmat, ...) {
        howeights <- exp(loghoweights)
        W <- c(0,1,howeights)[1L+nbmat]
        ## repeat last coefficient for higher orders without separate estimate
        W[is.na(W)] <- howeights[length(howeights)] # or set to 0 (see below)
        dim(W) <- dim(nbmat)
        dimnames(W) <- dimnames(nbmat)
        W
    }
    if (to0) { # set weights for order > maxlag to 0
        body(w)[[grep("is.na(W)", body(w), fixed=TRUE)]] <-
            quote(W[is.na(W)] <- 0)
    }
    
    dw <- function (loghoweights, nbmat, ...) {
        npar <- length(loghoweights)
        FUN <- function (nbOrder, weight)
            weight * (if (nbOrder==1L+npar) nbmat>=nbOrder else nbmat==nbOrder)
        mapply(FUN, 1L+seq_len(npar), exp(loghoweights),
               SIMPLIFY=FALSE, USE.NAMES=FALSE)
    }
    if (to0) {
        body(dw)[[grep("FUN <-", body(dw), fixed=TRUE)]] <-
            quote(FUN <- function (nbOrder, weight) weight * (nbmat==nbOrder))
    }

    ## for k=l, second derivative = first derivative, otherwise 0
    ## result of d2w must be a list of matrices of length npar*(npar+1L)/2L
    d2w <- dw
    if (maxlag > 2) { # i.e. npar > 1
        body(d2w)[[length(body(d2w))]] <-
            substitute(dw <- x, list(x=body(d2w)[[length(body(d2w))]]))

        body(d2w) <- as.call(c(as.list(body(d2w)), expression(
            d2wlength <- (npar^2+npar)/2,
            ## indices of diagonal elements in x[lower.tri(x,diag=TRUE)]
            d2wdiag <- c(1L,1L+cumsum(seq.int(npar,2L))),
            d2wlist <- rep.int(list(0*nbmat), d2wlength),
            d2wlist[d2wdiag] <- dw,
            d2wlist
            )))
    }

    ## Done
    environment(w) <- environment(dw) <- environment(d2w) <- .GlobalEnv
    list(w = w, dw = dw, d2w = d2w, initial = initial)
}

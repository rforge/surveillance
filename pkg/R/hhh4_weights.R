################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Helper functions for neighbourhood weight matrices in hhh4()
###
### Copyright (C) 2012 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


### check ne$weights specification

checkWeights <- function (weights, nUnits, nTime,
                          nbmat, data)  # only used for parametric weights
{
    name <- "control$ne$weights"

    ## check specification
    testweights <- if (is.array(weights)) weights else {
        if (is.list(weights) && checkWeightsFUN(weights)) {
            weights$w(weights$initial, nbmat, data)
        } else {
            stop("'", name, "' must be an array or a list of functions")
        }
    }
    
    ## check matrix/array of weights
    if (any(dim(testweights)[1:2] != nUnits) ||
        isTRUE(dim(testweights)[3] != nTime))
        stop("'", name, "' must conform to dimensions ",
             nUnits, " x ", nUnits, " (x ", nTime, ")")
    if (any(!diag(testweights) %in% 0))
        stop("'diag(", name, ")' must only contain zeroes")
    if (any(is.na(testweights)))
        stop("missing values in '", name, "' are not allowed")

    ## Done
    invisible(TRUE)
}



### calculate the weighted sum of counts of adjacent (or all other) regions
### i.e. the nTime x nUnit matrix with elements ne_ti = sum_j w_jit * y_jt
## W is either a nUnits x nUnits matrix of time-constant weights w_ji
## or a nUnits x nUnits x nTime array of time-varying weights

weightedSumNE <- function (observed, weights, lag)
{
  dimY <- dim(observed)
  nTime <- dimY[1L]
  nUnits <- dimY[2L]
  tY <- t(observed)
  
  timeconstantweights <- length(dim(weights)) == 2L
  selecti <- if (timeconstantweights) quote(weights[,i]) else quote(weights[,i,])
    
  res <- matrix(NA_real_, nrow=nTime, ncol=nUnits)
  for(i in seq_len(nUnits)){
    weights.i <- eval(selecti)
    weightedObs <- tY * weights.i
    res[,i] <- colSums(weightedObs, na.rm=TRUE)
  }

  rbind(matrix(NA_real_, lag, nUnits), head(res, nTime-lag))
}

## slower alternative, where the weights are always converted to a 3D array
weightedSumNE.old <- function(observed, weights, lag)
{
  dimY <- dim(observed)
  nTime <- dimY[1L]
  nUnits <- dimY[2L]
  
  nhood <- array(weights, c(nUnits,nUnits,nTime))
  
  res <- matrix(NA_real_, nrow=nTime,ncol=nUnits)
  for(i in seq_len(nUnits)){
    weights.i <- t(nhood[,i,])
    weightedObs <- observed * weights.i
    res[,i] <- rowSums(weightedObs, na.rm=TRUE)
  }
  
  rbind(matrix(NA_real_, lag, nUnits), head(res, nTime-lag))
}




### determine matrix with higher neighbourhood order given the binary matrix of
### first-order neighbours based on spdep::nblag()

nblagmat <- function (neighbourhood, maxlag = 1)
{
    stopifnot(isScalar(maxlag), maxlag > 0, is.matrix(neighbourhood))
    nd <- dim(neighbourhood)
    nregions <- nd[1L]
    if (nregions != nd[2L]) stop("'neighbourhood' matrix must be square")
    maxlag <- as.integer(min(maxlag, nregions-1)) # upper bound of nb order
    neighbourhood <- neighbourhood != 0           # convert to binary matrix
    ndn <- dimnames(neighbourhood)
    region.ids <- ndn[[1L]]
    
    if (maxlag == 1L) {
        storage.mode(neighbourhood) <- "integer"
        return(neighbourhood)
    }

    ## manually convert to spdep's "nb" class
    #nb <- apply(neighbourhood, 1, which)   # could result in a matrix
    region.idxs <- seq_len(nregions)
    nb <- lapply(region.idxs, function(i) region.idxs[neighbourhood[i,]])
    attr(nb, "region.id") <- region.ids
    class(nb) <- "nb"

    ## compute higher order neighbours using spdep::nblag()
    nb.lags <- spdep::nblag(nb, maxlag=maxlag)

    ## Side note: fast method to determine neighbours _up to_ specific order:
    ## crossprod(neighbourhoud) > 0  # second order neighbours (+set diag to 0)
    ## (neighbourhood %*% neighbourhood %*% neighbourhood) > 0  # order 3
    ## and so on...

    ## convert to a single matrix
    nbmat <- neighbourhood   # logical first-order matrix
    storage.mode(nbmat) <- "numeric"
    for (lag in 2:maxlag) {
        if (any(spdep::card(nb.lags[[lag]]) > 0L)) { # any neighbours of this order
            nbmat.lag <- spdep::nb2mat(nb.lags[[lag]], style="B",
                                       zero.policy=TRUE)
            nbmat <- nbmat + lag * nbmat.lag
        }
    }
    attr(nbmat, "call") <- NULL
    storage.mode(nbmat) <- "integer"

    ## message about maximum neighbour order by region
    maxlagbyrow <- apply(nbmat, 1, max)
    message("Note: range of maximum neighbour order by region is ",
            paste(range(maxlagbyrow), collapse="-"))

    ## Done
    nbmat
}






###############################################
### predefined parametric weight structures ###
###############################################


### check parametric weights specification consisting of a list of:
## - three functions: w, dw, and d2w
## - a vector of initial parameter values

checkWeightsFUN <- function (object)
{
    fnames <- paste0(c("","d","d2"), "w")
    if (any(!sapply(object[fnames], is.function)))
        stop("parametric weights require functions 'w', 'dw', 'd2'")
    if (any(!sapply(object[fnames], function(FUN) length(formals(FUN)) >= 3L)))
        stop("parametric weights functions must accept (not necessarily use)",
             "\n  at least 3 arguments (parameter vector, ",
             "neighbourhood order matrix, data)")
    if (!is.vector(object$initial, mode="numeric"))
        stop("parametric weights require initial parameter values")
    TRUE
}


### Construct weight matrix wji according to the Zeta-distribution with respect
### to the orders of neighbourhood (in nbmat, as e.g. obtained from nblagmat()),
### optionally fulfilling rowSums(wji) = 1
## As a formula (for j != i, otherwise wji = 0):
## - for shared=TRUE: wji = pzeta(oji; rho, maxlag) / sum_k I(ojk == oji)
## - for shared=FALSE: wji = pzeta(oji; rho, maxlag) / sum_k pzeta(ojk; rho, maxlag)
## Here, oji = oij is the order of nb of i and j,
## and pzeta(o; rho, m) = o^-rho / sum_{r=1}^m r^-rho is the Zeta-distribution
## on 1:m (also called Zipf's law).
## For shared=TRUE and normalize=FALSE, maxlag should not be greater than
## min_j(max_i(oji)), such that every region has neighbours up to order 'maxlag'
## and higher orders can not be infected. Otherwise, regions with not as
## high-order neighbours would not sum their weights to 1 (but lower).
## For shared=FALSE, maxlag=Inf yields the weights
## wji = oji^-\rho / sum_k ojk^-\rho
## In both cases, maxlag=1 yields the classical weights wji=1/nj.

zetaweights <- function (nbmat, maxlag = Inf, rho = 1,
                         normalize = TRUE, shared = FALSE)
{
    ## check maxlag
    if (!is.finite(maxlag)) maxlag <- max(nbmat)

    ## raw (non-normalized) zeta-distribution on 1:maxlag
    zetaweights <- c(0, seq_len(maxlag)^-rho)
    ##<- first 0 is for lag 0 (for instance, diag(nbmat))

    ## replace order by zetaweight of that order
    wji <- zetaweights[nbmat + 1L]
    wji[is.na(wji)] <- 0
    dim(wji) <- dim(nbmat)
    dimnames(wji) <- dimnames(nbmat)

    if (shared) {
        ## multiplicity of orders by row: dim(nbmat)==dim(multbyrow)
        multbyrow <- t(apply(nbmat, 1, function(x) table(x)[as.character(x)]))
        ## neighbours of same order share the zetaweight for that order
        wji <- wji / sum(zetaweights) / multbyrow
    }
    if (normalize) { # normalize such that each row sums to 1
        wji <- wji / rowSums(wji)
    }

    ## Done
    wji
}



### powerlaw weights
## in the non-truncated case, i.e. maxlag = max(nbmat),
## the raw powerlaw weights are defined as w_ji = o_ji^-rho,
## and with (row-)normalization we have    w_ji = o_ji^-rho / sum_k o_jk^-rho

powerlaw <- function (maxlag, normalize = TRUE) # only shared=FALSE is supported
{
    if (missing(maxlag)) {
        stop("'maxlag' must be specified (e.g., maximum neighbourhood order)")
    }

    ## main function which returns the weight matrix
    weights.call <- call("zetaweights", quote(nbmat), maxlag, quote(rho),
                         normalize, FALSE)
    weights <- as.function(c(alist(rho=, nbmat=, ...=), weights.call),
                           envir=.GlobalEnv)

    ## construct derivatives with respect to "rho"
    dweights <- d2weights <- as.function(c(alist(rho=, nbmat=, ...=),
        substitute({
            W <- weights.call
            is.na(nbmat) <- nbmat == 0L # w_jj = 0 => d/drho = 0, sum over j!=i
            tmp1a <- log(nbmat)
            norm <- rowSums(nbmat^-rho, na.rm=TRUE) # unused for raw weights
            tmp1b <- rowSums(nbmat^-rho * -log(nbmat), na.rm=TRUE)/norm # set to 0 for raw
            tmp1 <- tmp1a + tmp1b
            tmp2 <- rowSums(nbmat^-rho * log(nbmat)^2, na.rm=TRUE)/norm - tmp1b^2 # for 2nd deriv
            deriv <- W * -tmp1          # for d2weights: W * (tmp1^2 - tmp2)
            deriv[is.na(deriv)] <- 0
            deriv
        }, list(weights.call=weights.call))), envir=.GlobalEnv)

    ## adaptions for dweights and d2weights
    body(dweights)[[grep("^tmp2 <-", body(dweights))]] <- NULL
    body(d2weights)[[grep("^deriv <-", body(d2weights))]] <-
        quote(deriv <- W * (tmp1^2 - tmp2))
    
    ## simplifications for raw weights
    if (!normalize) {
        body(dweights)[[grep("^norm", body(dweights))]] <-
            body(d2weights)[[grep("^norm", body(d2weights))]] <- NULL
        body(dweights)[[grep("^tmp1b <-", body(dweights))]] <-
            body(d2weights)[[grep("^tmp1b <-", body(d2weights))]] <- quote(tmp1b <- 0)
        body(d2weights)[[grep("^tmp2 <-", body(d2weights))]] <- quote(tmp2 <- 0)
    }
    
    ## return list of functions
    list(w=weights, dw=dweights, d2w=d2weights, initial=1)
}



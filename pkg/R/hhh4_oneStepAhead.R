################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Compute one-step-ahead predictions (means) at a series of time points
###
### Copyright (C) 2011-2014 Michaela Paul and Sebastian Meyer
### $Revision$
### $Date$
################################################################################


oneStepAhead <- function(result, # hhh4-object (i.e. a hhh4 model fit)
                         tp,     # scalar: one-step-ahead predictions for time
                                 # points (tp+1):nrow(stsObj), or tp=c(from, to)
                         type = c("rolling", "first", "final"),
                         which.start = c("current", "final"), #if type="rolling"
                         keep.estimates = FALSE,
                         verbose = TRUE, # verbose-1 is used as verbose setting
                                         # for sequentially refitted hhh4 models
                         cores = 1) # if which.start="final", the predictions
                                    # can be computed in parallel
{
    stopifnot(inherits(result, c("ah4", "hhh4")))
    type <- match.arg(type)
    which.start <- if (type == "rolling") match.arg(which.start) else "final"
    if (cores > 1 && which.start == "current")
        stop("no parallelization for \"rolling\" if 'which.start=\"current\"'")
    startfinal <- hhh4coef2start(result)

    ## get model terms
    model <- result[["terms"]]
    if (is.null(model))
        model <- result$terms <- with(result, interpretControl(control, stsObj))
    nTime <- model$nTime
    nUnits <- model$nUnits
    
    ## check that tp is within the time period of the data
    maxlag <- if (is.null(result$lags) || all(is.na(result$lags)))
        1L else max(result$lags, na.rm=TRUE)
    stopifnot(tp %in% seq.int(maxlag,nTime-1L), length(tp) %in% 1:2)
    if (length(tp) == 1) tp <- c(tp, max(model$subset)-1)
    tps <- tp[1]:tp[2]
    ntps <- length(tps)
    observed <- model$response[tps+1,,drop=FALSE]
    rownames(observed) <- tps+1

    ## adjust verbosity for model refitting
    verbose <- as.integer(verbose)
    result$control$verbose <- max(0, verbose - (ntps>1))
    if (type != "rolling" && verbose > 1L) verbose <- 1L
    do_pb <- verbose == 1L
    
    ## initialize result
    pred <- matrix(NA_real_, nrow=ntps, ncol=nUnits,
                   dimnames=list(tps+1, colnames(observed)))
    psi <- if (model$nOverdisp > 0) {
        psiNames <- grep("overdisp", names(model$initialTheta), value=TRUE)
        matrix(NA_real_, nrow=ntps, ncol=model$nOverdisp,
               dimnames=list(tps, psiNames))
    } else NULL
    if (keep.estimates) {
	coefficients <- matrix(NA_real_,
                               nrow=ntps, ncol=length(model$initialTheta),
                               dimnames=list(tps, names(model$initialTheta)))
	Sigma.orig <- matrix(NA_real_, nrow=ntps, ncol=model$nSigma,
                             dimnames=list(tps, names(result$Sigma.orig)))
        logliks <- matrix(NA_real_, nrow=ntps, ncol=2L,
                          dimnames=list(tps, c("loglikelihood", "margll")))
    } else {
        coefficients <- Sigma.orig <- logliks <- NULL
    }

    ## initial fit
    fit <- if (type == "first") {
        if (do_pb)
            cat("\nRefitting model at first time point t =", tps[1L], "...\n")
        update.hhh4(result, subset.upper = tps[1L], start = startfinal,
                    keep.terms = TRUE) # need "model" -> $terms
    } else result
    if (!fit$convergence) stop("initial fit did not converge")
    
    if (cores > 1L && requireNamespace("parallel")) {
        stop("parallelization is not implemented yet")
        ## parallel::mclapply(seq_along(tps), 
        ##                    mc.preschedule=TRUE, mc.cores=cores)
    } else { ## sequential one-step ahead predictions
        if (do_pb) pb <- txtProgressBar(min=0, max=ntps, initial=0, style=3)
        for(i in seq_along(tps)) {
            if (verbose > 1L) {
                cat("\nOne-step-ahead prediction @ t =", tps[i], "...\n")
            } else if (do_pb) setTxtProgressBar(pb, i)
            
            if (type == "rolling") { # update fit
                fit.old <- fit # backup
                fit <- update.hhh4(result, subset.upper=tps[i],
                                   start=switch(which.start,
                                                current=hhh4coef2start(fit),
                                                final=startfinal),
                                   keep.terms=TRUE) # need "model" -> $terms
                if (!fit$convergence) {
                    if (do_pb) cat("\n")
                    cat("WARNING: No convergence @ t =", tps[i], "!\n")
                    ## FIXME: do a grid search ?
                    fit <- fit.old
                    next
                }
            }
            
            coefs <- fit$coefficients
            pred[i,] <- meanHHH(coefs, fit$terms,
                                subset=tps[i]+1L, total.only=TRUE)
            if (model$nOverdisp > 0)
                psi[i,] <- coefs[psiNames]
            if (keep.estimates) {
                coefficients[i,] <- coefs
                Sigma.orig[i,] <- fit$Sigma.orig
                logliks[i,] <- c(fit$loglikelihood, fit$margll)
            }
        }
        if (do_pb) close(pb)
    }
    
    list(pred=pred, observed=observed, psi=psi, allConverged=all(!is.na(pred)),
         coefficients=coefficients, Sigma.orig=Sigma.orig, logliks=logliks)
}

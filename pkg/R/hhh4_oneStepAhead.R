################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Compute one-step-ahead predictions (means) at a series of time points
###
### Copyright (C) 2011-2013 Michaela Paul and Sebastian Meyer
### $Revision$
### $Date$
################################################################################


oneStepAhead <- function(result, # ah4-object (i.e. a hhh4 model fit)
                         tp,     # scalar: one-step-ahead predictions for time
                                 # points (tp+1):nrow(stsObj), or tp=c(from, to)
                         type = c("rolling", "first", "final"),
                         which.start = c("current", "final"), #if type="rolling"
                         keep.estimates = FALSE,
                         verbose = TRUE) # verbose-1 is used as verbose setting
                                         # for sequentially refitted hhh4 models
{
    stopifnot(inherits(result, "ah4"))
    type <- match.arg(type)
    which.start <- if (type == "rolling") match.arg(which.start) else "final"
    startfinal <- ah4coef2start(result)

    ## get model terms
    model <- result[["terms"]]
    if (is.null(model))
        model <- result$terms <- with(result, interpretControl(control, stsObj))
    nTime <- model$nTime
    nUnits <- model$nUnits
    
    ## check that tp is within the time period of the data
    stopifnot(tp %in% seq_len(nTime-1L), length(tp) %in% 1:2)
    if (length(tp) == 1) tp <- c(tp, max(model$subset)-1)
    tps <- tp[1]:tp[2]
    ntps <- length(tps)
    observed <- model$response[tps+1,,drop=FALSE]
    rownames(observed) <- tps+1

    ## adjust verbosity for model refitting
    verbose <- as.integer(verbose)
    result$control$verbose <- max(0, verbose - (ntps>1))
    if (type != "rolling" && verbose > 1L) verbose <- 1L
    
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

    ## sequential one-step ahead predictions
    fit <- result
    do_pb <- verbose == 1L
    if (do_pb) pb <- txtProgressBar(min=0, max=ntps, initial=0, style=3)
    for(i in seq_along(tps)) {
        if (verbose > 1L || (do_pb && type=="first" && i==1)) {
            cat("\nOne-step-ahead prediction @ t =", tps[i], "...\n")
        } else if (do_pb) setTxtProgressBar(pb, i) 
        if (type == "rolling" || (type == "first" && i == 1L)) {
            fit.old <- fit # backup
            fit <- update.ah4(result, subset.upper=tps[i],
                              start = switch(which.start,
                                             current=ah4coef2start(fit),
                                             final=startfinal),
                              keep.terms=TRUE) # need "model" -> $terms
        }
        if (fit$convergence) {
            coefs <- coef(fit, reparamPsi=FALSE)
            pred[i,] <- meanHHH(coefs, fit$terms,
                                subset=tps[i]+1, total.only=TRUE)
            if (model$nOverdisp > 0)
                psi[i,] <- coefs[psiNames]
            if (keep.estimates) {
                coefficients[i,] <- coefs
                Sigma.orig[i,] <- getSdCorr(fit)
                logliks[i,] <- c(fit$loglikelihood, fit$margll)
            }
        } else {
            if (do_pb) cat("\n")
            switch(type,
                   rolling = {
                       cat("WARNING: No convergence @ t =", tps[i], "!\n")
                       ## FIXME: do a grid search ?
                       fit <- fit.old
                   },
                   first = stop("no convergence at first time point t=",tps[i]),
                   final = stop("input is no valid hhh4()-fit (not converged)")
                   )
        }
    }
    if (do_pb) close(pb)
    
    list(pred=pred, observed=observed, psi=psi, allConverged=all(!is.na(pred)),
         coefficients=coefficients, Sigma.orig=Sigma.orig, logliks=logliks)
}

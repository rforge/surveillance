################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Standard methods for hhh4-fits
###
### Copyright (C) 2010-2014 Michaela Paul and Sebastian Meyer
### $Revision$
### $Date$
################################################################################


print.hhh4 <- function (x, digits = max(3, getOption("digits") - 3),reparamPsi=TRUE, ...) 
{
  if(!x$convergence)
    cat('Results are not reliable! Try different starting values. \n')
  else {
    if(!is.null(x$call)){
      cat("\nCall: \n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
	}

    if (length(ranef(x))) {
      cat('Random effects: \n')
      V <- as.matrix(round(diag(getCov(x)),digits=digits))
      corr <- round(getCov(x),digits=digits)
      corr[upper.tri(corr,diag=TRUE)] <- ""
      V.corr <- cbind(V,corr)
      colnames(V.corr) <- c("Var","Corr",rep("",ncol(V.corr)-2))
      print(noquote(V.corr))

	  cat("\nFixed effects:\n")
	  print.default(format(fixef(x, reparamPsi=TRUE,...), digits = digits), print.gap = 2, 
		  quote = FALSE)
    } else if (length(fixef(x))) {
        cat("Coefficients:\n")
        print.default(format(fixef(x, reparamPsi=TRUE,...), digits = digits), print.gap = 2, 
            quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
  }
}
getCov <- function(x){
  Sigma <- x$Sigma
  corr <- cov2cor(Sigma)
  diag(corr) <- diag(Sigma)
  rownames(corr) <- colnames(corr) <-
      sub("^sd\\.","",names(x$Sigma.orig))[grep("^sd\\.",names(x$Sigma.orig))]
  return(corr)
}

summary.hhh4 <- function(object, ...){
  # do not summarize results in case of non-convergence
  if(!object$convergence){
    cat('Results are not reliable! Try different starting values. \n')
	return(invisible(object))
  }

  ret <- object[c("call","convergence","dim","loglikelihood","margll","nTime","nUnit")]

  # get estimated covariance matrix of random effects
  if(object$dim["random"]>0){
	REmat <- object$Sigma
	attr(REmat, "correlation") <- cov2cor(REmat)
	attr(REmat, "sd") <- sqrt(diag(REmat))
    rownames(REmat) <- colnames(REmat) <- gsub("sd.", "", names(object$Sigma.orig))[grep("sd.", names(object$Sigma.orig))]

	aic <- NULL
	bic <- NULL
  } else{
	REmat <- NULL
	aic <- AIC(object)
        bic <- BIC(object)
  }

  fe <- fixef(object,se=TRUE, ...)
  re <- ranef(object)

  ret <- c(ret, list(fixef=fe, ranef=re, REmat=REmat, AIC=aic, BIC=bic))
  class(ret) <- "summary.hhh4"
  return(ret)
}

print.summary.hhh4 <- function(x, digits = max(3, getOption("digits") - 3), ...){
  if(!x$convergence){
    cat('Results are not reliable! Try different starting values. \n')
	invisible(x)
  } else {
    if(!is.null(x$call)){
      cat("\nCall: \n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    }
    
    if(x$dim["random"]>0){
      cat('Random effects: \n')
      V <- as.matrix(round(diag(x$REmat),digits=digits))
      corr <- round(attr(x$REmat, "correlation"),digits=digits)
      corr[upper.tri(corr,diag=TRUE)] <- ""
      V.corr <- cbind(V,corr)
      colnames(V.corr) <- c("Var","Corr",rep("",ncol(V.corr)-2))
      print(noquote(V.corr))
    }
        
    cat('\nFixed effects: \n')
    print(round(x$fixef,digits=digits),print.gap=2)

    if(x$dim["random"]==0){
      cat('\nlog-likelihood:   ',round(x$loglikelihood,digits=digits-2),'\n')  
      cat('AIC:              ',round(x$AIC,digits=digits-2),'\n')
      cat('BIC:              ',round(x$BIC,digits=digits-2),'\n\n')
    } else {
      cat('\npenalized log-likelihood: ',round(x$loglikelihood,digits=digits-2),'\n')  
      cat('marginal log-likelihood:  ',round(x$margll,digits=digits-2),'\n\n')        
    }
    
    cat('Number of units:         ',x$nUnit,'\n')
    cat('Number of time points:   ',x$nTime,'\n\n')
    
    if(any(x$fixef[,"Estimates"]== -20)){
      cat("Warning: coefficients \'",rownames(x$fixef)[x$fixef[,"Estimates"]==-20] ,"\' could not be estimated accurately\n\n")
    }
  }
  
  invisible(x)
}

terms.hhh4 <- function (x, ...)
{
    if (is.null(x$terms))
        interpretControl(x$control,x$stsObj) else x$terms
}

nobs.hhh4 <- function (object, ...) {
    if (object$convergence) object$nObs else NA_real_
}

logLik.hhh4 <- function(object, ...)
{
    val <- if (object$convergence) object$loglikelihood else {
        warning("algorithm did not converge")
        NA_real_
    }
    attr(val, "df") <- if (object$dim["random"])
        NA_integer_ else object$dim[["fixed"]]  # use "[[" to drop the name
    attr(val, "nobs") <- nobs(object)
    class(val) <- "logLik"
    val
}

AIC.hhh4 <- function (object, ..., k = 2)
{
    if (object$dim["random"]) {
        warning("AIC not well defined for models with random effects")
        NA_real_
    } else NextMethod("AIC")
}
BIC.hhh4 <- function (object, ...)
{
    if (object$dim["random"]) {
        warning("BIC not well defined for models with random effects")
        NA_real_
    } else NextMethod("BIC")
}

coef.hhh4 <- function(object, se=FALSE,
                      reparamPsi=TRUE, idx2Exp=NULL, amplitudeShift=FALSE, ...)
{
  if (object$control$family == "Poisson") reparamPsi <- FALSE
  coefs <- object$coefficients
  stdErr <- object$se
  
  if (reparamPsi) {
    #extract psi coefficients
    index <- grep("-log(overdisp",names(coefs), fixed=TRUE)

    if (length(index) == 0L) { # backward compatibility (internal psi coef
                               # was named "overdisp" prior to r406)
      index <- grep("^overdisp", names(coefs))
    } else {
      psi.names <- names(coefs)[index]
      # change labels from "-log(overdisp.xxx)" to "overdisp.xxx"
      names(coefs)[index] <- substr(psi.names, start=6, stop=nchar(psi.names)-1)
    }
    
    #transform psi coefficients
    coefs[index] <- exp(-coefs[index])
    # se's using Delta rule: se[h(psi)] = se[psi] * |h'(psi)|
    # h = exp(-psi), h' = -exp(-psi)
    D <- diag(coefs[index],length(index))
    stdErr[index] <- sqrt(diag(D %*% object$cov[index,index] %*% t(D)))
  }
  
  if(length(idx2Exp)){
    # extract coefficients on log-scale
    exp.names <- names(coefs)[idx2Exp]
    # change labels
    names(coefs)[idx2Exp] <- paste0("exp(",exp.names,")")
    
    # transform
    coefs[idx2Exp] <- exp(coefs[idx2Exp])
    D <- diag(coefs[idx2Exp],length(idx2Exp))
    stdErr[idx2Exp] <- sqrt(diag(D %*% object$cov[idx2Exp,idx2Exp] %*% t(D)))
  }
  
  
  if(amplitudeShift){
    indexAS <- sort(c(grep(".sin",names(coefs),fixed=TRUE),
                      grep(".cos",names(coefs),fixed=TRUE)))
    namesSinCos <- names(coefs)[indexAS]
    namesSinCos <- gsub(".sin",".A",namesSinCos)
    namesSinCos <- gsub(".cos",".s",namesSinCos)
    names(coefs)[indexAS] <- namesSinCos
    coefs[indexAS] <- sinCos2amplitudeShift(coefs[indexAS])
    D <- jacobianAmplitudeShift(coefs[indexAS])
    cov <- D %*% object$cov[indexAS,indexAS] %*% t(D)
    stdErr[indexAS] <- sqrt(diag(cov))
  }

  if (se) cbind("Estimates"=coefs, "Std. Error"=stdErr) else coefs
}

vcov.hhh4 <- function (object,
                       reparamPsi=TRUE, idx2Exp=NULL, amplitudeShift=FALSE, ...)
{
    if (object$control$family == "Poisson") reparamPsi <- FALSE
    coefs <- object$coefficients
    dim <- length(coefs)
    coefnames <- names(coefs)

    ## coefficient indexes
    idxPsi <- grep("-log(overdisp", coefnames, fixed=TRUE)
    idxAS <- sort(c(grep(".sin(", coefnames, fixed=TRUE),
                    grep(".cos(", coefnames, fixed=TRUE)))
    ## index sets must be disjoint
    idxOccupied <- c(if (reparamPsi) idxPsi,
                     if (amplitudeShift) idxAS)
    idxOverlap <- intersect(idxOccupied, idx2Exp)
    if (length(idxOverlap)) {
        warning("following 'idx2Exp' were ignored due to overlap: ",
                paste(idxOverlap, collapse=", "))
        idx2Exp <- setdiff(idx2Exp, idxOverlap)
    }

    ## Use multivariate Delta rule => D %*% vcov %*% t(D), D: Jacobian.
    ## For idx2Exp and reparamPsi, we only transform coefficients independently,
    ## i.e. D is diagonal (with elements 'd')
    d <- rep.int(1, dim)
    if (reparamPsi) {
        ## h = exp(-psi), h' = -exp(-psi)
        d[idxPsi] <- -exp(-coefs[idxPsi])
        coefnames[idxPsi] <- substr(coefnames[idxPsi], start=6,
                                    stop=nchar(coefnames[idxPsi])-1L)
    }
    if (length(idx2Exp)) { # h = exp(coef), h' = exp(coef)
        d[idx2Exp] <- exp(coefs[idx2Exp])
        coefnames[idx2Exp] <- paste0("exp(", coefnames[idx2Exp], ")")
    }
    ## For the amplitude/shift-transformation, D is non-diagonal
    vcov <- if (amplitudeShift) {
        D <- diag(d, dim)
        D[idxAS,idxAS] <-
            jacobianAmplitudeShift(sinCos2amplitudeShift(coefs[idxAS]))
        coefnames[idxAS] <- gsub(".sin",".A",coefnames[idxAS],fixed=TRUE)
        coefnames[idxAS] <- gsub(".cos",".s",coefnames[idxAS],fixed=TRUE)
        D %*% object$cov %*% t(D)
    } else t(t(object$cov*d)*d)  # 30 times faster than via matrix products
        
    dimnames(vcov) <- list(coefnames, coefnames)
    vcov
}


fixef.hhh4 <- function(object,...){
  if(object$dim[1]>0){
    return(head(coef(object,...), object$dim[1]))
  } else return(NULL)
}

ranef.hhh4 <- function(object, tomatrix = FALSE, ...){
  if(object$dim[2]>0){
    ranefvec <- tail(coef(object,...), object$dim[2])
  } else return(NULL)
  if (!tomatrix) return(ranefvec)

  ## transform to a nUnits x c matrix (c %in% 1:3)
  model <- terms(object)
  idxRE <- model$indexRE
  idxs <- unique(idxRE)
  names(idxs) <- model$namesFE[idxs]
  mat <- sapply(idxs, function (idx) {
      RE <- ranefvec[idxRE==idx]
      Z <- model$terms["Z.intercept",][[idx]]
      "%m%" <- get(model$terms["mult",][[idx]])
      Z %m% RE
  })
  rownames(mat) <- colnames(model$response)
  return(mat)
}

confint.hhh4 <- function (object, parm, level = 0.95,
                          reparamPsi=TRUE, idx2Exp=NULL, amplitudeShift=FALSE, ...)
{
    cf <- coef(object, reparamPsi=reparamPsi, idx2Exp=idx2Exp,
               amplitudeShift=amplitudeShift, se=TRUE)
    pnames <- rownames(cf)
    if (missing(parm)) 
        parm <- pnames#[seq_len(object$dim["fixed"])]  # confints for fixef only
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3),"%")
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ses <- cf[parm,2]
    ci[] <- cf[parm,1] + ses %o% fac
    ci
}


## mean predictions for a subset of 1:nrow(object$stsObj)
predict.hhh4 <- function(object, newSubset=object$control$subset, type="response", ...)
{
    if (type == "response" &&
        all((m <- match(newSubset, object$control$subset, nomatch=0L)) > 0)) {
        ## we can extract fitted means from object
        object$fitted.values[m,,drop=FALSE]
    } else { ## means for time points not fitted (not part of object$control$subset)
        predicted <- meanHHH(coef(object, reparamPsi=FALSE),
                             terms(object),
                             subset=newSubset)
        if (type=="response") predicted$mean else {
            type <- match.arg(type, names(predicted))
            predicted[[type]]
        }
    }
}


### refit hhh4-model
## ...: arguments modifying the original control list
## subset.upper: refit on a subset of the data up to that time point
## use.estimates: use fitted parameters as new start values
##                (only applicable if same model)

update.hhh4 <- function (object, ..., subset.upper=NULL, use.estimates=FALSE)
{
    control <- object$control
    control <- modifyList(control, list(...))
    if (isScalar(subset.upper))
        control$subset <- control$subset[control$subset <= subset.upper]
    if (use.estimates)
        control$start <- hhh4coef2start(object)
    hhh4(object$stsObj, control)
}


## convert fitted parameters to a list suitable for control$start
hhh4coef2start <- function (fit)
    list(fixed = fixef(fit, reparamPsi=FALSE),
         random = ranef(fit, reparamPsi=FALSE),
         sd.corr = getSdCorr(fit))

## character vector of model components that are "inModel"
componentsHHH4 <- function (object)
    names(which(sapply(object$control[c("ar", "ne", "end")], "[[", "inModel")))

## deviance residuals
residuals.hhh4 <- function (object, type = c("deviance", "response"), ...)
{
    type <- match.arg(type)
    obs <- observed(object$stsObj)[object$control$subset,]
    fit <- fitted(object)
    if (type == "response")
        return(obs - fit)

    ## deviance residuals
    ## Cf. residuals.ah, it calculates:
    ## deviance = sign(y - mean) * sqrt(2 * (distr(y) - distr(mean)))
    ## pearson = (y - mean)/sqrt(variance)
    dev.resids <- switch(object$control$family,
        "Poisson" = poisson()$dev.resids,
        "NegBin1" = {
            psi <- exp(object$coefficients[object$dim[1L]])
            negative.binomial(psi)$dev.resids
        },
        "NegBinM" = {
            psicoefidx <- seq.int(to=object$dim[1L],
                                  length.out=object$nUnit)
            psi <- matrix(
                exp(object$coefficients[psicoefidx]),
                object$nTime, object$nUnit, byrow=TRUE)
            negative.binomial(psi)$dev.resids # CAVE: non-standard use
        },
        stop("not implemeted for \"", object$control$family, "\"-models"))

    di2 <- dev.resids(y=obs, mu=fit, wt=1)
    sign(obs-fit) * sqrt(pmax.int(di2, 0))
}


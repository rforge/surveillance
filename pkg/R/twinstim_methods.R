################################################################################
### Methods for objects of class "twinstim", specifically:
### coef (coefficients), vcov, logLik, print, summary, print.summary, plot
### Author: Sebastian Meyer
################################################################################

coef.twinstim <- function (object, ...)
{
    object$coefficients
}

# asymptotic variance-covariance matrix (inverse of fisher information matrix)
vcov.twinstim <- function (object, ...)
{
    solve(object$fisherinfo)  # inverse of estimated expected fisher information
}

logLik.twinstim <- function (object, ...)
{
    r <- object$loglik
    attr(r, "df") <- length(coef(object))
    class(r) <- "logLik"
    r
}

print.twinstim <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n")
    print.default(x$call)
    cat("\nCoefficients:\n")
    print.default(format(coef(x), digits=digits), print.gap = 2, quote = FALSE)
    cat("\nLog-likelihood: ", format(logLik(x), digits=digits), "\n", sep = "")
    if (!x$converged) {
        cat("\nWARNING: OPTIMIZATION ROUTINE DID NOT CONVERGE!\n")
    }
    cat("\n")
    invisible(x)
}

summary.twinstim <- function (object, test.iaf = FALSE,
    correlation = FALSE, symbolic.cor = FALSE, ...)
{
    ans <- object[c("call", "converged", "counts")]
    ans$cov <- vcov(object)
    npars <- object$npars
    coefs <- coef(object)
    nbeta0 <- npars[1]; p <- npars[2]; nbeta <- nbeta0 + p
    q <- npars[3]
    nNotIaf <- nbeta + q
    niafpars <- npars[4] + npars[5]
    est <- coefs
    se <- sqrt(diag(ans$cov))
    zval <- est/se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    coefficients <- cbind(est, se, zval, pval)
    dimnames(coefficients) <- list(names(est),
        c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    ans$coefficients.beta <- coefficients[seq_len(nbeta),,drop=FALSE]
    ans$coefficients.gamma <- coefficients[nbeta+seq_len(q),,drop=FALSE]
    ans$coefficients.iaf <- coefficients[nNotIaf+seq_len(niafpars),,drop=FALSE]
    if (!test.iaf) {
        ## usually, siaf and tiaf parameters are strictly positive,
        ## or parametrized on the logscale. In this case the usual wald test
        ## with H0: para=0 is invalid or meaningless.
        is.na(ans$coefficients.iaf[,3:4]) <- TRUE
    }
    # estimated parameter correlation
    if (correlation) {
        ans$correlation <- cov2cor(ans$cov)
        ans$symbolic.cor <- symbolic.cor
    }
    ans$loglik <- logLik(object)
    ans$aic <- AIC(object)
    ans$runtime <- object$runtime
    class(ans) <- "summary.twinstim"
    ans
}

print.summary.twinstim <- function (x,
    digits = max(3, getOption("digits") - 3), symbolic.cor = x$symbolic.cor,
    signif.stars = getOption("show.signif.stars"), ...)
{
    nbeta <- nrow(x$coefficients.beta) # = nbeta0 + p
    q <- nrow(x$coefficients.gamma)
    niafpars <- nrow(x$coefficients.iaf)
    cat("\nCall:\n")
    print.default(x$call)
    if (nbeta > 0L) {
        cat("\nCoefficients of the endemic component:\n")
        printCoefmat(x$coefficients.beta, digits = digits,
            signif.stars = signif.stars, signif.legend = (q==0L) && signif.stars, ...)
    } else cat("\nNo coefficients in the endemic component.\n")
    if (q > 0L) {
        cat("\nCoefficients of the epidemic component:\n")
        printCoefmat(rbind(x$coefficients.gamma, x$coefficients.iaf), digits = digits,
            signif.stars = signif.stars, ...)
#         if (niafpars > 0L) {
#             #cat("Coefficients of interaction functions:\n")
#             printCoefmat(x$coefficients.iaf, digits = digits, signif.stars = signif.stars, ...)
#         }
    } else cat("\nNo epidemic component.\n")
#     nEvents <- x$nEvents
#     nh0 <- length(nEvents)
#     if (nh0 < 2L) {
#         cat("\nTotal number of infections: ", nEvents, "\n")
#     } else {
#         cat("\nBaseline intervals:\n")
#         intervals <- character(nh0)
#         for(i in seq_len(nh0)) {
#             intervals[i] <-
#             paste("(",
#                   paste(format(x$intervals[c(i,i+1L)],trim=TRUE), collapse=";"),
#                   "]", sep = "")
#         }
#         names(intervals) <- paste("logbaseline", seq_len(nh0), sep=".")
#         print.default(rbind("Time interval" = intervals,
#                             "Number of events" = nEvents),
#                       quote = FALSE, print.gap = 2)
#     }
#     cat("\n", attr(x$aic, "type"), ": ", format(x$aic, digits=max(4, digits+1)),
#         if (!attr(x$aic, "exact")) "\t(simulated penalty weights)" else "",
#         sep = "")
    cat("\nAIC: ", format(x$aic, digits=max(4, digits+1)))
    cat("\nLog-likelihood:", format(x$loglik, digits = digits))
    cat("\nNumber of log-likelihood evaluations:", x$counts[1])
    cat("\nNumber of score function evaluations:", x$counts[2])
    if (!is.null(x$runtime)) {
        cat("\nRuntime:", format(x$runtime, digits=max(4, digits+1)), "seconds")
    }
    cat("\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1L) {
        cat("\nCorrelation of Coefficients:\n")
        if (is.logical(symbolic.cor) && symbolic.cor) {
            correl <- symnum(correl, abbr.colnames = NULL)
            correlcodes <- attr(correl, "legend")
            attr(correl, "legend") <- NULL
            print(correl)
            cat("---\nCorr. codes:  ", correlcodes, "\n", sep="")
        } else {
            correl <- format(round(correl, 2), nsmall = 2, digits = digits)
            correl[!lower.tri(correl)] <- ""
            print(correl[-1, -p, drop = FALSE], quote = FALSE)
        }
        }
    }
    if (!x$converged) {
        cat("\nWARNING: OPTIMIZATION ROUTINE DID NOT CONVERGE!\n")
    }
    cat("\n")
    invisible(x)
}



### 'cat's the summary in LaTeX code

toLatex.summary.twinstim <- function (object, digits = max(3, getOption("digits") - 3),
                                      align = "rrrrr", withAIC = TRUE, ...)
{
ret <- capture.output({
    cat("\\begin{tabular}{", align, "}\n\\hline\n", sep="")
    cat(" & Estimate & Std. Error & $z$ value & $\\P(|Z|>|z|)$ \\\\\n\\hline\n\\hline\n")

    tabh <- object$coefficients.beta
    tabe <- rbind(object$coefficients.gamma, object$coefficients.iaf)
    for (tabname in c("tabh", "tabe")) {
        tab <- get(tabname)
        if (nrow(tab) > 0L) {
            rownames(tab) <- gsub(" ", "", rownames(tab))
            tab_char <- capture.output(printCoefmat(tab,digits=digits,signif.stars=FALSE,na.print=""))[-1]
            tab_char <- sub("([<]?)[ ]?([0-9]+)e([+-][0-9]+)$", "\\1\\2\\\\cdot{}10^{\\3}", tab_char)
            con <- textConnection(tab_char)
            tab2 <- read.table(con, colClasses="character")
            close(con)
            parnames <- paste0("\\texttt{",tab2[,1],"}")
            tab2 <- as.data.frame(lapply(tab2[,-1], function(x) paste0("$",x,"$")))
            rownames(tab2) <- parnames
            print(xtable::xtable(tab2), only.contents=TRUE, hline.after=NULL,
                  include.colnames=FALSE, sanitize.text.function=identity)
            cat("\\hline\n")
        }
    }
    if (withAIC) {
        cat("\\hline\n")
        cat("AIC:& $", format(object$aic, digits=max(4, digits+1)), "$ &&&\\\\\n")
        cat("Log-likelihood:& $", format(object$loglik, digits=digits), "$ &&&\\\\\n")
    }
    cat("\\hline\n")
    cat("\\end{tabular}\n")
})
class(ret) <- "Latex"
ret
}


### Plot evolution of the intensity

intensityplot.twinstim <- function (x, which = c("epidemic proportion", "endemic proportion", 
    "total intensity"), aggregate = c("time", "space"), ...)
{
    which <- match.arg(which)
    aggregate <- match.arg(aggregate)
    
    stop("not yet implemented")
    
}


### Plot fitted tiaf or siaf(cbind(0, r)), r=distance

iafplot <- function (object, which = c("siaf", "tiaf"),
    types = 1:nrow(object$qmatrix),
    conf.type = if (length(pars) > 1) "bootstrap" else "parbounds",
    conf.level = 0.95, conf.B = 999,
    ngrid = 101, col.estimate = rainbow(length(types)), col.conf = col.estimate,
    alpha.B = 0.15, lwd = c(3,1), lty = c(1,2), xlim = c(0,eps), ylim = c(0,1),
    add = FALSE, xlab = NULL, ylab = NULL, ...)
{
    which <- match.arg(which)
    eps <- object$medianeps[if (which=="siaf") "spatial" else "temporal"]
    FUN <- object$formula[[which]][[if (which=="siaf") "f" else "g"]]
    coefs <- coef(object)
    idxpars <- grep(which,names(coefs))
    pars <- coefs[idxpars]
    force(conf.type)
    if (length(pars) == 0 || any(is.na(conf.type)) || is.null(conf.type)) {
        conf.type <- "none"
    }
    conf.type <- match.arg(conf.type, choices = c("parbounds", "bootstrap", "none"))
    
    if (!add) {
        if (is.null(xlab)) xlab <- if (which == "siaf") {
            expression("Distance " * group("||",bold(s)-bold(s)[j],"||") * " from host")
        } else {
            expression("Distance " * t-t[j] * " from host")
        }
        if (is.null(ylab)) ylab <- if (which == "siaf") {
            expression(f(group("||",bold(s)-bold(s)[j],"||")))
        } else {
            expression(g(t-t[j]))
        }
        plot(xlim, ylim, type="n", xlab = xlab, ylab = ylab, ...)
    }
    xgrid <- seq(0, xlim[2], length.out=ngrid)
    
    for (i in seq_along(types)) {
        ## select parameters on which to evaluate iaf
        parSample <- switch(conf.type, parbounds = {
            cis <- confint(object, idxpars, level=conf.level)
            ## all combinations of parameter bounds
            do.call("expand.grid", as.data.frame(t(cis)))
        }, bootstrap = {
            ## bootstrapping parameter values
            if (length(pars) == 1) {
                as.matrix(c(pars, rnorm(conf.B, mean=pars,
                                sd=sqrt(vcov(object)[idxpars,idxpars]))))
            } else {
                rbind(pars, mvtnorm::rmvnorm(conf.B, mean=pars,
                                 sigma=vcov(object)[idxpars,idxpars,drop=FALSE]))
            }
        })
        
        ## add confidence limits
        if (!is.null(parSample)) {
            fvalsSample <- apply(parSample, 1, function(pars)
                                 FUN(if(which=="siaf") cbind(xgrid,0) else xgrid,
                                     pars, types[i]))
            lowerupper <- if (conf.type == "parbounds") {
                t(apply(fvalsSample, 1, range))
            } else { # bootstrapped parameter values
                if (is.na(conf.level)) {
                    stopifnot(alpha.B >= 0, alpha.B <= 1)
                    .col <- col2rgb(col.conf[i], alpha=TRUE)[,1]
                    .col["alpha"] <- round(alpha.B*.col["alpha"])
                    .col <- do.call("rgb", args=c(as.list(.col), maxColorValue = 255))
                    matlines(x=xgrid, y=fvalsSample, type="l", lty=lty[2],
                             col=.col, lwd=lwd[2]) # returns NULL
                } else {
                    t(apply(fvalsSample, 1, quantile,
                            probs=c(0,conf.level) + (1-conf.level)/2))
                }
            }
            if (!is.null(lowerupper)) {
                matlines(x=xgrid, y=lowerupper, type="l", lty=lty[2],
                         col=col.conf[i], lwd=lwd[2])
            }
        }
        
        ## add point estimate
        lines(x=xgrid,
              y=FUN(if(which=="siaf") cbind(xgrid,0) else xgrid, pars, types[i]),
              lty=lty[1], col=col.estimate[i], lwd=lwd[1])
    }
    invisible()
}


### Plot method for twinstim (wrapper for plotiaf and intensityplot)

plot.twinstim <- function (x, which, ...)
{
    cl <- match.call()
    which <- match.arg(which, choices =
                       c(eval(formals(intensityplot.twinstim)$which),
                         eval(formals(iafplot)$which)))
    FUN <- if (which %in% eval(formals(intensityplot)$which))
        "intensityplot" else "iafplot"
    cl[[1]] <- as.name(FUN)
    eval(cl, envir = parent.frame())
}

### set defaults for 'which' in 'plot.twinstim'
## formals(plot.twinstim)$which <- as.call(as.list(c(as.name("c"),
##     c(eval(formals(intensityplot.twinstim)$which), eval(formals(iafplot)$which)))))



### Calculates the basic reproduction number R0 for individuals
### with marks given in 'newevents' (defaults to all-zero marks)

R0.twinstim <- function (object, newevents, dimyx = spatstat.options("npixel"), ...)
    {
        npars <- object$npars
        if (npars["q"] == 0L) stop("no epidemic component in model")
        typeNames <- rownames(object$qmatrix)
        nTypes <- length(typeNames)
        types <- 1:nTypes
        form <- formula(object)
        epidemic <- form$epidemic
        Fcircle <- form$siaf$Fcircle
        G <- form$tiaf$G
        coefs <- coef(object)

        # epidemic predictor
        .mmehack <- FALSE
        if (missing(newevents)) {
            newevents <- data.frame(type = factor(types, levels=types, labels=typeNames),
                eps.s = object$medianeps[["spatial"]],
                eps.t = object$medianeps[["temporal"]]
            )
            rownames(newevents) <- newevents$type
            newevents[setdiff(all.vars(epidemic),names(newevents))] <- 0
            .mmehack <- TRUE
            #gamma0 <- coefs[grep("^e\\.(\\(Intercept\\)|type.+)", names(coefs))]
        } else if (is.data.frame(newevents) && is.factor(newevents$type)) {
            newevents$type <- factor(newevents$type, levels = typeNames)
        } else {
            stop("missing factor variable 'type' in data.frame 'newevents'")
        }
        epidemic <- terms(epidemic, data = newevents, keep.order = TRUE)
        #environment(epidemic) <- globalenv()  # empty environment would not work with model.frame
        mme <- model.matrix(epidemic,
            model.frame(epidemic, data=newevents, na.action=na.pass, drop.unused.levels = FALSE))
        gamma <- coefs[sum(npars[1:2]) + seq_len(npars["q"])]
        if (.mmehack) {   # append further 0 columns (for dummies of other factor marks)
            mme <- cbind(mme, matrix(0, nrow(mme), length(gamma)-ncol(mme)))
        } else if (ncol(mme) != length(gamma)) {
            stop("epidemic model matrix of event marks has the wrong length (check the variable types in 'newevents' (factors, etc))")
        }
        gammapred <- drop(exp(mme %*% gamma))

        # convert types of newevents to integer codes
        newevents$type <- as.integer(newevents$type)

        # integrals of interaction functions for all combinations of type and eps.s/eps.t in newevents
        siafpars <- coefs[sum(npars[1:3]) + seq_len(npars["nsiafpars"])]
        tiafpars <- coefs[sum(npars[1:4]) + seq_len(npars["ntiafpars"])]
        typeScombis <- expand.grid(type=types, eps.s=unique(newevents$eps.s), KEEP.OUT.ATTRS=FALSE)
        typeTcombis <- expand.grid(type=types, eps.t=unique(newevents$eps.t), KEEP.OUT.ATTRS=FALSE)
        typeScombis$fInt <- apply(typeScombis, MARGIN=1, FUN=function (type_eps.s) {
            type <- type_eps.s[1]
            eps.s <- type_eps.s[2]
            if (is.null(Fcircle)) {
                polyCub.midpoint(discpoly(c(0,0), eps.s), form$siaf$f,
                                 siafpars, type, dimyx = dimyx)
            } else {
                Fcircle(eps.s, siafpars, type)
            }
        })
        typeTcombis$gInt <- G(typeTcombis$eps.t, tiafpars, typeTcombis$type) - G(rep.int(0,nTypes), tiafpars, types)[typeTcombis$type]
        # match combinations to rows in 'newevents'
        neweventscombiidxS <- match(with(newevents,paste(type,eps.s,sep=".")),
                                  with(typeScombis,paste(type,eps.s,sep=".")))
        neweventscombiidxT <- match(with(newevents,paste(type,eps.t,sep=".")),
                                  with(typeTcombis,paste(type,eps.t,sep=".")))

        # qSum
        qSumTypes <- rowSums(object$qmatrix)

        # return R0 values for events in newevents
        gammapred * qSumTypes[newevents$type] * typeScombis$fInt[neweventscombiidxS] * typeTcombis$gInt[neweventscombiidxT]
    }


######################################################################
# Plot Kolmogorov-Smirnov residual plot
#
# Parameters:
#  object - a fitted twinstim model
#
# Draws the transformed residuals together with backtransformed
# 95% Kolmogorov-Smirnov error bounds.
######################################################################

residuals.twinstim <- function(object, plot = TRUE, ...)
{
  #cumulative intensities
  tau <- object$tau

  #Transform to uniform variable
  Y <- diff(tau) # Y <- diff(c(0,tau))
  U <- sort(1-exp(-Y))

  #Calculate KS test
  ks <- stats::ks.test(U,"punif",exact=TRUE,alternative="two.sided")
  
  #return value
  ret <- list(tau=tau, U=U, ks=ks)
  
  #Ready for plotting
  if (plot) {
    ks.plot.unif(U, ...)
    invisible(ret)
  } else {
    ret
  }
}


######################################################################
# Function to compute estimated and profile likelihood based
# confidence intervals. Heavy computations might be necessary!
#
#Params:
# fitted - output from a fit with twinstim
# profile - list with 4D vector as entries - format:
#               c(index, lower, upper, grid size)
#           where index is the index in the coef vector
#                 lower and upper are the parameter limits (can be NA)
#                 grid size is the grid size of the equally spaced grid
#                 between lower and upper (can be 0)
# alpha - (1-alpha)% profile likelihood CIs are computed.
#         If alpha <= 0 then no CIs are computed
# control - control object to use for optim in the profile loglik computations
#
# Returns:
#  list with profile loglikelihood evaluations on the grid
#  and highest likelihood and wald confidence intervals
######################################################################

profile.twinstim <- function (fitted, profile, alpha = 0.05,
    control = list(fnscale = -1, factr = 1e1, maxit = 100), do.ltildeprofile=FALSE,...)
{
  ## Check that input is ok
  profile <- as.list(profile)
  if (length(profile) == 0L) {
    stop("nothing to do")
  }
  lapply(profile, function(one) {
    if (length(one) != 4L) {
      stop("each profile entry has to be of form ",
           "'c(index, lower, upper, grid size)'")
    }})
  if (is.null(fitted[["functions"]])) {
    stop("'fitted' must contain the component 'functions' -- fit using the option model=TRUE")
  }

  ################################################################
  warning("Sorry, the profile likelihood is not implemented yet.")
  ###############################################################

  ## Control of the optim procedure
  if (is.null(control[["fnscale",exact=TRUE]])) { control$fnscale <- -1 }
  if (is.null(control[["maxit",exact=TRUE]])) { control$maxit <- 100 }
  if (is.null(control[["trace",exact=TRUE]])) { control$trace <- 2 }


  ## Estimated normalized likelihood function
  ltildeestim <- function(thetai,i) {
    theta <- theta.ml
    theta[i] <- thetai
    fitted$functions$ll(theta) - loglik.theta.ml
  }

  ## Profile normalized likelihood function
  ltildeprofile <- function(thetai,i)
  {
    #cat("Investigating theta[",i,"] = ",thetai,"\n")

    emptyTheta <- rep(0, length(theta.ml))

    # Likelihood l(theta_{-i}) = l(theta_i, theta_i)
    ltildethetaminusi <- function(thetaminusi) {
      theta <- emptyTheta
      theta[-i] <- thetaminusi
      theta[i] <- thetai
      #cat("Investigating theta = ",theta,"\n")
      res <- fitted$functions$ll(theta) - loglik.theta.ml
      #cat("Current ltildethetaminusi value: ",res,"\n")
      return(res)
    }
    # Score function of all params except thetaminusi
    stildethetaminusi <- function(thetaminusi) {
      theta <- emptyTheta
      theta[-i] <- thetaminusi
      theta[i] <- thetai
      res <- fitted$functions$sc(theta)[-i]
      #cat("Current stildethetaminusi value: ",res,"\n")
      return(res)
    }

    # Call optim -- currently not adapted to arguments of control arguments
    # used in the fit
    resOthers <- tryCatch(
            optim(par=theta.ml[-i], fn = ltildethetaminusi, gr = stildethetaminusi,
                  method = "BFGS", control = control),
            warning = function(w) print(w), error = function(e) list(value=NA))
    resOthers$value
  }



  ## Initialize
  theta.ml <- coef(fitted)
  loglik.theta.ml <- c(logLik(fitted))
  se <- sqrt(diag(vcov(fitted)))
  resProfile <- list()


  ## Perform profile computations for all requested parameters
  cat("Evaluating the profile logliks on a grid...\n")
  for (i in 1:length(profile))
    {
    cat("i= ",i,"/",length(profile),"\n")
    #Index of the parameter in the theta vector
    idx <- profile[[i]][1]
    #If no borders are given use those from wald intervals (unconstrained)
    if (is.na(profile[[i]][2])) profile[[i]][2] <- theta.ml[idx] - 3*se[idx]
    if (is.na(profile[[i]][3])) profile[[i]][3] <- theta.ml[idx] + 3*se[idx]
    #Evaluate profile loglik on a grid (if requested)
    if (profile[[i]][4] > 0) {
      thetai.grid <- seq(profile[[i]][2],profile[[i]][3],length=profile[[i]][4])
      resProfile[[i]] <- matrix(NA, nrow = length(thetai.grid), ncol = 4L,
        dimnames = list(NULL, c("grid","profile","estimated","wald")))

      #Loop over all gridpoints
      for (j in 1:length(thetai.grid)) {
        cat("\tj= ",j,"/",length(thetai.grid),"\n")
        resProfile[[i]][j,] <- c(thetai.grid[j],
           #Do we need to compute ltildeprofile (can be quite time consuming)
           ifelse(do.ltildeprofile, ltildeprofile(thetai.grid[j],idx), NA),
           ltildeestim(thetai.grid[j],idx),
           - 1/2*(1/se[idx]^2)*(thetai.grid[j] - theta.ml[idx])^2)
      }
    }
  }
  names(resProfile) <- names(theta.ml)[sapply(profile, function(x) x[1L])]

  ###############################
  ## Profile likelihood intervals
  ###############################
  # Not done, yet
  ciProfile <- NULL

  ####Done, return
  return(list(lp=resProfile, ci.hl=ciProfile, profileObj=profile))
}



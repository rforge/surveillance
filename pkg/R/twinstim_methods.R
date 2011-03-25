################################################################################
### Methods for objects of class "twinstim", specifically:
### coef (coefficients), vcov, logLik, print, summary, print.summary,
# plot
###
### Author: Sebastian Meyer
### $Date: 2010-11-16 04:08:22 +0100 (Tue, 16 Nov 2010) $
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

summary.twinstim <- function (object,
    correlation = FALSE, symbolic.cor = FALSE, ...)
{
    ans <- object[c("call", "converged", "counts")] #, "intervals", "nEvents"
    ans$cov <- vcov(object)
    npars <- object$npars
    coefs <- coef(object)
    nbeta0 <- npars[1]; p <- npars[2]; nbeta <- nbeta0 + p
    q <- npars[3]
    nNotIaf <- nbeta + q
    niafpars <- npars[4] + npars[5]
    est <- coefs #[1:nNotIaf]
    se <- sqrt(diag(ans$cov)) #[1:nNotIaf])
    zval <- est/se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    coefficients <- cbind(est, se, zval, pval)
    dimnames(coefficients) <- list(names(est),
        c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    ans$coefficients.beta <- coefficients[seq_len(nbeta),,drop=FALSE]
    ans$coefficients.gamma <- coefficients[nbeta+seq_len(q),,drop=FALSE]
    ans$coefficients.iaf <- coefficients[nNotIaf+seq_len(niafpars),,drop=FALSE]
    # estimated parameter correlation
    if (correlation) {
        ans$correlation <- cov2cor(ans$cov)
        ans$symbolic.cor <- symbolic.cor
    }
    ans$loglik <- logLik(object)
    ans$aic <- AIC(object) #extractAIC(object, ...)
#     ans$aic <- as.vector(aic[2L])   # remove 'edf' element
#     attributes(ans$aic) <- attributes(aic)[c("type", "exact")]
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

toLatex.summary.twinstim <- function (object, digits = max(3, getOption("digits") - 3), align = "rrrrr", ...)
{
library("xtable")
ret <- capture.output({
    cat("\\begin{tabular}{", align, "}\n\\hline\n", sep="")
    cat(" & Estimate & Std. Error & $z$ value & $\\P(|Z|>|z|)$ \\\\\n\\hline\n\\hline\n")

    tabh <- object$coefficients.beta
    tabe <- rbind(object$coefficients.gamma, object$coefficients.iaf)
    for (tabname in c("tabh", "tabe")) {
        tab <- get(tabname)
        if (nrow(tab) > 0L) {
            rownames(tab) <- gsub(" ", "", rownames(tab))
            tab_char <- capture.output(printCoefmat(tab,digits=digits,signif.stars=FALSE))[-1]
            tab_char <- sub("([<]?)[ ]?([0-9]+)e([+-][0-9]+)$", "\\1\\2\\\\cdot{}10^{\\3}", tab_char)
            con <- textConnection(tab_char)
            tab2 <- read.table(con, colClasses="character")
            close(con)
            parnames <- paste("\\texttt{",tab2[,1],"}",sep="")
            tab2 <- as.data.frame(lapply(tab2[,-1], function(x) paste("$",x,"$",sep="")))
            rownames(tab2) <- parnames
            print(xtable(tab2), only.contents = TRUE, include.colnames = FALSE, sanitize.text.function = identity, hline.after = NULL)
            cat("\\hline\n")
        }
    }
    cat("\\hline\n")
    cat("AIC:& $", format(object$aic, digits=max(4, digits+1)), "$ &&&\\\\\n")
    cat("Log-likelihood:& $", format(object$loglik, digits=digits), "$ &&&\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
})
class(ret) <- "Latex"
ret
}



### Plots fitted tiaf or isotropic siaf

plotiaf <- function (twinstim, iaf = c("siaf", "tiaf"),
    types = 1:nrow(twinstim$qmatrix), xlim = c(0,eps), ylim = c(0,1),
    cols = rainbow(length(types)), add = FALSE, ...)
{
    iaf <- match.arg(iaf)
    eps <- twinstim$medianeps[if (iaf=="siaf") "spatial" else "temporal"]
    FUN <- twinstim$formula[[iaf]][[if (iaf=="siaf") "f" else "g"]]
    coefs <- coef(twinstim)
    idxpars <- grep(iaf,names(coefs))
    pars <- coefs[idxpars]
    cis <- confint(twinstim, idxpars)
    if (!add) plot(xlim, ylim, type="n", ...)
    for (i in seq_along(types)) {
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, pars, types[i]), add=TRUE, col=cols[i])
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, cis[,1], types[i]), add = TRUE, lty=2, col=cols[i])
        curve(FUN(if(iaf=="siaf") cbind(x,0) else x, cis[,2], types[i]), add = TRUE, lty=2, col=cols[i])
    }
}



### Calculates the basic reproduction number R0 for individuals with all marks equal to 0

#Generic function from RLadyBug to compute R0 for a SIR like model
#Copy/paste to here to make R0 calculations for twinstim model work.
if ( ! isGeneric( "R0" ) ){
  fun <- function( object, ... ) standardGeneric( "R0" )
  setGeneric( "R0", fun )
}

setOldClass("twinstim")
setMethod("R0", signature(object = "twinstim"),
    function (object, newevents, dimyx = spatstat.options("npixel"))
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
)

######################################################################
# Plot Kolmogorov-Smirnov residual plot
#
# Parameters:
#  m - a fitted twinstim model
#
# Draws the transformed residuals together with backtransformed
# 95% Kolmogorov-Smirnov error bounds.
######################################################################

KSPlot <- function(m) {
  tau <- m$tau
  n <- length(tau)

  #Figure 10 in Ogata (1988)
  Y <- diff(tau) # Y <- diff(c(0,tau))
  U <- sort(1-exp(-Y))

  #Helper function to invert KS test. pkolmogorov2x is the CDF of
  #the Kolmogorov test statistic
  f <- function(x,p) {
    1 - .C("pkolmogorov2x", p = as.double(x), as.integer(n), PACKAGE = "stats")$p - p
  }

  #Small helper function to draw a line
  myabline <- function(a,b,x.grid,...) {
    lines(x.grid, a + b * x.grid, ...)
  }

  #Test inversion
  D95 <- uniroot(f,lower=0,upper=0.1,p=0.05)$root
  D99 <- uniroot(f,lower=0,upper=0.1,p=0.01)$root

  #Ready for plotting, but don't produce the plot yet, just set up the
  #scene
  plot(U, ecdf(U)(U),xlab=expression(u[i]),ylab="Cumulative distribution",type="n")
  rug(U)

  col <- "gray"
  myabline(a=0,b=1,x.grid=seq(0,1,length=1000),col=col,lwd=2)
  lines(U, ecdf(U)(U),type="s")

  myabline(a=D95,b=1,x.grid=seq(0,1,length=1000),col=col,lty=2)
  myabline(a=-D95,b=1,x.grid=seq(0,1,length=1000),col=col,lty=2)
  #myabline(a=D99,b=1,x.grid=seq(0,1,length=1000),col=col,lty=2)
  #myabline(a=-D99,b=1,x.grid=seq(0,1,length=1000),col=col,lty=2)
  legend(x="topleft",lty=2,col=col,"95% KS error bounds")
  #Done
  invisible()
}

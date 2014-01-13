#######################################
### Hook functions for package start-up
#######################################

gpclibCheck <- function (fatal = TRUE)
{
    gpclibOK <- surveillance.options("gpclib")
    if (!gpclibOK && fatal) {
        message("Note: The gpclib license is accepted by ",
                sQuote("surveillance.options(gpclib=TRUE)"), ".")
        stop("acceptance of the gpclib license is required")
    }
    gpclibOK
}

.onLoad <- function (libname, pkgname)
{
    ## initialize options
    reset.surveillance.options()
}

.onAttach <- function (libname, pkgname)
{
    ## Startup message
    VERSION <- packageVersion(pkgname, lib.loc=libname)
    packageStartupMessage("This is ", pkgname, " ", VERSION, ". ",
                          "For overview type ",
                          sQuote(paste0("help(", pkgname, ")")), ".")

    ## decide if we should run all examples (some take a few seconds)
    allExamples <- if (interactive()) TRUE else {
        .withTimings <- Sys.getenv("_R_CHECK_TIMINGS_")
        withTimings <- !is.na(.withTimings) && nzchar(.withTimings)
        !withTimings
    }
    surveillance.options(allExamples = allExamples)
}


### Function 'base::rep_len' only exists as of R version 3.0.0
### Define it as a wrapper for base::rep() for older versions

if (getRversion() < "3.0.0" || !exists("rep_len", baseenv())) {
    rep_len <- function (x, length.out) rep(x, length.out=length.out)
}



###########################
### Little helper functions
###########################


### checking if x is scalar, i.e. a numeric vector of length 1.

isScalar <- function (x) {
    length(x) == 1L && is.vector(x, mode = "numeric")
}


### _c_onditional lapply, which only uses lapply() if X really is a list object
### and otherwise applies FUN to X. The result is always a list (of length 1 in
### the latter case). Used for neOffset in hhh4 models.

clapply <- function (X, FUN, ...)
{
    if (is.list(X)) lapply(X, FUN, ...) else list(FUN(X, ...))
}


### pretty p-value formatting

formatPval <- function (pv, eps = 1e-4)
{
    format1 <- function (p)
        format.pval(p, digits = if (p<10*eps) 1 else 2, eps = eps)
    sapply(pv, format1)
}



###############################################################
### backwards-compatibility for old class name "ah4" (<= 1.7-0)
###############################################################

local({
    methods17 <- c("print", "summary", "print.summary", "terms", "logLik",
                   "AIC", "coef", "fixef", "ranef", "confint", "predict",
                   "update", "plot", "simulate")
    for (generic in methods17) {
        methodname <- paste(generic, "hhh4", sep=".")
        method <- get(methodname)
        ## mark ah4-method as deprecated (as of next major release)
        ## body(method) <- as.call(append(
        ##     as.list(body(method)),
        ##     substitute(
        ##         .Deprecated(new,
        ##                     msg=c(
        ##                     "Since surveillance 1.8-0, hhh4()-results are of",
        ##                     " class \"hhh4\" instead of \"ah4\".",
        ##                     "\nOld \"ah4\"-methods will be removed.")),
        ##         list(new=methodname)),
        ##     after=1L))
        assign(paste(generic, "ah4", sep="."), method, pos=parent.frame(2L))
    }
})

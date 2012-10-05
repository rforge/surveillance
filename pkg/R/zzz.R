#######################################
### Hook functions for package start-up
#######################################

## .onLoad <- function (libname, pkgname)
## {
##   #Load the CIdata thing
##   #data("CIdata", package=pkgname)

##   #Read the table of the hypgeom_2F1 function for parameters c(1/3,2/3) and
##   #5/3 -- atm this is computed for the values seq(0,10,by=0.01) and 11:100
##   #Load the pre-evaluated Hypergeometric function for computing Anscombe residuals
##   #data("hypGeomSmall",package=pkgname)

##   # <- those data sets should not be loaded into .GlobalEnv and be visible to the user!
##   # moved CIdata to (internal) sysdata
## }

.onAttach <- function (libname, pkgname)
{      
    ## Startup message
    vrs <- packageDescription(pkgname, lib.loc = libname, fields = "Version", drop = TRUE)
    packageStartupMessage("This is ", pkgname, " ", vrs, ". ",
                          "For overview type ",
                          sQuote(paste("?", pkgname, sep="")), ".")
    
    ## License limitation for package gpclib
    packageStartupMessage(paste(
        "\n\tNote: polygon geometry computations related to",
        "\n\tthe \"epidataCS\" class in ", pkgname, " depend on",
        "\n\tthe package gpclib, which has a restricted licence.\n",
        #"(Free for non-commercial use; commercial use prohibited.)",
        sep=""), appendLF = FALSE)
}



###########################
### Little helper functions
###########################


### checking if x is scalar, i.e. a numeric vector of length 1.

isScalar <- function (x) {
    length(x) == 1L && is.vector(x, mode = "numeric")
}


### returns the dot/scalar product of two vectors

dotprod <- function (x,y)
{
    sum(x*y)
}


### Function 'base::paste0()' only exists as of R version 2.15.0
### Define it as a wrapper for base::paste() for older versions

if (getRversion() < "2.15.0" || R.version$"svn rev" < 57795 ||
    !exists("paste0", baseenv())) {
    paste0 <- function (..., collapse = NULL) {
        ## the naive way: paste(..., sep = "", collapse = collapse)
        ## probably better: establish appropriate paste() call:
        cl <- match.call()
        names(cl) <- sub("sep", "", names(cl)) # no sep argument
        cl$sep <- ""
        cl[[1]] <- as.name("paste")
        eval(cl, envir = parent.frame())
    }
}


### Function which modifies a list _call_ according to another one similar to
### what the function utils::modifyList (by Deepayan Sarkar) does for list objects
## modifyListcall is used by update.twinstim

is.listcall <- function (x)
{
    is.call(x) &&
    as.character(x[[1]]) %in% c("list", "alist")
}

modifyListcall <- function (x, val)
{
    stopifnot(is.listcall(x), is.listcall(val))
    xnames <- names(x)[-1]
    for (v in names(val)[nzchar(names(val))]) {
        xv <- if (v %in% xnames && is.listcall(x[[v]]) && is.listcall(val[[v]]))
            modifyListcall(x[[v]], val[[v]]) else val[[v]]
        x[v] <- list(xv)                # allows for NULL value of val[[v]]
    }
    x
}



### _c_onditional lapply, which only uses lapply() if X really is a list object
### and otherwise applies FUN to X. The result is always a list (of length 1 in
### the latter case). Used for neOffset in hhh4 models.

clapply <- function (X, FUN, ...)
{
    if (is.list(X)) lapply(X, FUN, ...) else list(FUN(X, ...))
}



### Simple wrapper around functionality of the numDeriv and maxLik packages
### to check the score vector and the Fisher information matrix
### CAVE: the return values of both wrappers are not unified

checkDerivatives.numDeriv <- function(ll, score, fisher, par,
                                      method="Richardson",
                                      method.args=list(), ...)
{
    cat("Checking analytical score vector using numDeriv::grad() ...\n")
    nsc <- numDeriv::grad(ll, par, method, method.args, ...)
    asc <- score(par, ...)
    print(all.equal(asc, nsc, check.attributes=FALSE))
    cat("Checking analytical Fisher information matrix using numDeriv::hessian() ...\n")
    if (length(par) > 50)
        cat("NOTE: this might take several minutes considering length(par) =",
            length(par), "\n")
    nfi <- -numDeriv::hessian(ll, par, "Richardson", method.args, ...)
    afi <- fisher(par, ...)
    print(all.equal(afi, nfi, check.attributes=FALSE))
    invisible(list(score  = list(analytic=asc, numeric=nsc),
                   fisher = list(analytic=afi, numeric=nfi)))
}


checkDerivatives.maxLik <- function(ll, score, fisher, par, eps=1e-6,
                                    print=FALSE, ...)
{
    cat("Checking analytical score and Fisher using maxLik::compareDerivatives() ...\n")
    res <- maxLik::compareDerivatives(
                   f=ll, grad=score,
                   hess=function (par, ...) -fisher(par, ...),
                   t0=par, eps=eps, print=print, ...)
    cat("Comparison of score vectors:\n")
    print(all.equal(res$compareGrad$analytic, drop(res$compareGrad$numeric),
                    check.attributes=FALSE))
    cat("Comparison of Fisher information matrices:\n")
    print(all.equal(res$compareHessian$analytic, drop(res$compareHessian$numeric),
                    check.attributes=FALSE))
    invisible(res)
}

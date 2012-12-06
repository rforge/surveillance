#######################################
### Hook functions for package start-up
#######################################

gpclibPermitStatus <- function ()
{
    ## check global gpclib permission status
    globally <- isTRUE(getOption("gpclib"))

    ## check for permission via spatstat package
    via.spatstat <- spatstat.options("gpclib")

    ## check for permission via maptools
    via.maptools <- if ("maptools" %in% loadedNamespaces())
        maptools::gpclibPermitStatus() else FALSE

    ## return gpclib permission status
    globally | via.spatstat | via.maptools
}

gpclibCheck <- function (fatal = TRUE)
{
    gpclibOK <- isTRUE(gpclibPermitStatus()) && requireNamespace("gpclib")
    if (!gpclibOK && fatal) {
        message("The gpclib license is considered accepted by setting ",
                sQuote("options(gpclib=TRUE)"), ".")
        stop("the gpclib package and acceptance of its license is required")
    }
    gpclibOK
}

.onAttach <- function (libname, pkgname)
{      
    ## Startup message
    vrs <- packageDescription(pkgname, lib.loc = libname, fields = "Version", drop = TRUE)
    packageStartupMessage("This is ", pkgname, " ", vrs, ". ",
                          "For overview type ",
                          sQuote(paste0("help(", pkgname, ")")), ".")

    ## License limitation for package gpclib
    if (!gpclibCheck(fatal=FALSE)) packageStartupMessage(
        "Note: Polygon geometry computations required for the generation of",
        "\n      \"epidataCS\" objects currently depend on the gpclib package,",
        "\n      which has a restricted license.",
        "\n      This functionality is disabled by default, but may be enabled",
        "\n      by setting ", sQuote("options(gpclib=TRUE)"),
                 " if this license is applicable."
    )
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


### _c_onditional lapply, which only uses lapply() if X really is a list object
### and otherwise applies FUN to X. The result is always a list (of length 1 in
### the latter case). Used for neOffset in hhh4 models.

clapply <- function (X, FUN, ...)
{
    if (is.list(X)) lapply(X, FUN, ...) else list(FUN(X, ...))
}




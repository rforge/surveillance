################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Space-time K-function analysis of "epidataCS" objects
### along the lines of Diggle et al (1995):
### "Second-order analysis of space-time clustering" (Stat Methods Med Res)
###
### Copyright (C) 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

## call K-function methods in package "splancs"
stKcall <- function (which = c("stkhat", "stsecal", "stmctest"),
                     object, eps.s, eps.t, ...)
{
    stopifnot(inherits(object, "epidataCS"))
    
    ## get the function
    which <- match.arg(which)
    FUN <- get(which, mode = "function", envir = getNamespace("splancs"))
    
    ## default arguments
    commonArgs <- list(
        pts = coordinates(object$events), times = object$events$time,
        poly = NULL,                      tlimits = summary(object)$timeRange,
        s = eps.s,                        tm = eps.t
    )
    args <- modifyList(commonArgs, list(...))
    if (is.null(args$poly)) { # use coordinates of first polygon
        if (length(object$W) > 1L || length(object$W@polygons[[1]]@Polygons) > 1L)
            stop("package \"splancs\" does not support multi-'poly'gons")
        args$poly <- coordinates(object$W@polygons[[1L]]@Polygons[[1L]])
    }
    if (which == "stmctest" && is.null(args[["nsim"]])) {
        args$nsim <- 199L
    }
    
    ## unfortunately, argument names are not consistent across functions
    if (which == "stsecal")
        names(args)[names(args) == "tlimits"] <- "tlim"
    if (which == "stmctest")
        names(args)[names(args) == "tm"] <- "tt"

    ## call the selected splancs function
    do.call(FUN, args)
}

## Monte-Carlo test for space-time interaction 
stKtest <- function (object, eps.s = NULL, eps.t = NULL, B = 199,
                     cores = 1, seed = NULL, poly = object$W)
{
    stopifnot(inherits(object, "epidataCS"),
              isScalar(cores), cores > 0, isScalar(B), B > 0)
    cores <- as.integer(cores)
    B <- as.integer(B)
    
    ## naive default grids
    if (is.null(eps.s))
        eps.s <- seq(0, min(object$events$eps.s, apply(bbox(object$W), 1, diff)/2),
                     length.out = 10)
    if (is.null(eps.t))
        eps.t <- seq(0, min(object$events$eps.t, tail(object$stgrid$stop,1L)/2),
                     length.out = 10)
    
    ## extract coordinates of the polygon
    polycoordslist <- xylist(poly)
    if (length(polycoordslist) > 1L) {
        stop("package \"splancs\" does not support multi-'poly'gons")
    }
    Wcoords <- as.matrix(as.data.frame(polycoordslist[[1L]]))

    ## calculate K-function
    stK <- stKcall("stkhat", object = object, eps.s = eps.s, eps.t = eps.t,
                   poly = Wcoords)

    ## calculate standard error
    seD <- stKcall("stsecal", object = object, eps.s = eps.s, eps.t = eps.t,
                   poly = Wcoords)

    ## perform Monte Carlo permutation test (parallelized)
    permt <- plapply(
        X = diff(round(seq(from = 0, to = B, length.out = cores + 1L))),
        FUN = function (nsim) {
            stKcall("stmctest", object = object, eps.s = eps.s, eps.t = eps.t,
                    poly = Wcoords, nsim = nsim, quiet = TRUE)[["t"]]
        },
        .parallel = cores, .seed = seed, .verbose = FALSE
    )
    mctest <- list(
        "t0" = sum(stK$kst - outer(stK$ks, stK$kt)),
        "t" = unlist(permt, recursive = FALSE, use.names = FALSE)
    )
    PVAL <- mean(c(mctest[["t0"]], mctest[["t"]]) >= mctest[["t0"]])

    ## return test results
    structure(
        list(method = "Diggle et al (1995) K-function test for space-time clustering",
             data.name = deparse(substitute(object)),
             statistic = setNames(mctest$t0, "U"), # sum of residuals
             parameter = setNames(B, "B"), p.value = PVAL,
             pts = coordinates(object$events),
             stK = stK, seD = seD, mctest = mctest),
        class = c("stKtest", "htest")
    )
}

plot.stKtest <- function (x, Dzero = FALSE, ...)
{
    splancs::stdiagn(pts = x$pts, stkh = x$stK, stse = x$seD, stmc = x$mctest,
                     Dzero = Dzero)
    invisible()
}

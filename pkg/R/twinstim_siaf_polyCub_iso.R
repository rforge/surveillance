################################################################################
### C-Level Cubature of "siaf" over Polygonal Domains
###
### Copyright (C) 2017 Sebastian Meyer
###
### This file is part of the R package "surveillance",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
################################################################################

## 'polys' is a list of polygons in the form of owin$bdry
## 'intrfr_name' identifies the function used in the integrand
## 'pars' is a vector of parameters for "intrfr"
siaf_polyCub_iso <- function (polys, intrfr_name, pars, control = list())
{
    ## default control arguments for polyCub_iso / Rdqags
    ## similar to args(stats::integrate)
    control <- modifyList(
        list(subdivisions = 100L, rel.tol = .Machine$double.eps^0.25,
             stop.on.error = TRUE),
        control)
    if (is.null(control[["abs.tol"]]))
        control$abs.tol <- control$rel.tol
    ## integrate over each polygon
    ints <- lapply(X = polys, FUN = siaf_polyCub1_iso,
                   intrfr_code = INTRFR_CODE[intrfr_name], pars = pars,
                   subdivisions = control$subdivisions,
                   rel.tol = control$rel.tol,
                   abs.tol = control$abs.tol,
                   stop.on.error = control$stop.on.error)
    sum(unlist(ints, recursive = FALSE, use.names = FALSE))
}

## 'xypoly' is a list(x, y) of vertex coordinates (open)
siaf_polyCub1_iso <- function (xypoly, intrfr_code, pars,
                               subdivisions = 100L,
                               rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
                               stop.on.error = TRUE)
{
    if (length(xypoly$y) != (L <- length(xypoly$x)))
        stop("xypoly$x and xypoly$y must have equal length")
    .C("twinstim_siaf_polyCub_iso",
       as.double(xypoly$x), as.double(xypoly$y), as.integer(L),
       as.integer(intrfr_code), as.double(pars),
       as.integer(subdivisions), as.double(abs.tol), as.double(rel.tol),
       as.integer(stop.on.error),
       value = double(1L), abserr = double(1L), neval = integer(1L),
       PACKAGE = "surveillance")$value
}

INTRFR_CODE <- c(
    "intrfr.powerlaw" = 10L,
    "intrfr.powerlaw.dlogsigma" = 11L,
    "intrfr.powerlaw.dlogd" = 12L
    )

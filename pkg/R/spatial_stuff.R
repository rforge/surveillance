################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Spatial helper functions
###
### Copyright (C) 2009-2013 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


### sample n points uniformly on a disc with radius r

runifdisc <- function (n, r = 1)
{
    rangle <- runif(n, 0, 2*pi)
    rdist <- r * sqrt(runif(n, 0, 1))
    rdist * cbind(cos(rangle), sin(rangle))
}


### Redefinition of the gpclib::scale.poly method for gpc.poly's to also do centering

scale.gpc.poly <-
    function (x, center = c(0,0), scale = c(1,1)) {
        x@pts <- lapply(x@pts, function (p) {
            p$x <- (p$x-center[1]) / scale[1]
            p$y <- (p$y-center[2]) / scale[2]
            p
        })
        x
    }


### Same as inside.owin for gpc.poly (using point.in.polygon from package sp)

inside.gpc.poly <- function(x, y = NULL, polyregion, mode.checked = FALSE)
{
    xy <- xy.coords(x, y, recycle=FALSE)
    N <- length(xy$x)
    # check for each polygon of polyregion if points are in the polygon
    locations <- sapply(polyregion@pts, function (poly) {
        pip <- point.in.polygon(xy$x, xy$y, poly$x, poly$y, mode.checked = mode.checked)
        if (poly$hole) { # if point is inside a hole then attribute -Inf
            ifelse(pip == 1, -Inf, 0)
        } else pip
    })
    inside <- if (N == 1L) sum(locations) > 0 else rowSums(locations) > 0
    return(inside)
}


### Count number of instances at the same location of a SpatialPoint object

multiplicity.default <- function (x, ...)
{
    distmat <- as.matrix(dist(x))
    as.integer(rowSums(distmat == 0))
}

multiplicity.Spatial <- function (x, ...)
{
    multiplicity(coordinates(x))
}


### Compute the intersection of a "gpc.poly" with a "discpoly" using gpclib

intersectCircle <- function (Wgpc, center, r, npoly)
{
    ## gpclibCheck() # unexported function, check already in caller
    circle <- discpoly(center = center, r = r, npoly = npoly,
                       class = "gpc.poly", hole = FALSE)
    intersection <- gpclib::intersect(circle, Wgpc)  # this order seems to be faster
    scale(intersection, center = center) # use scale method as defined above
}


### determine matrix with higher neighbourhood order based on spdep::nblag()
### given the binary matrix of first-order neighbours

nbOrder <- function (neighbourhood, maxlag = 1)
{
    if (!requireNamespace("spdep"))
        stop("package ", dQuote("spdep"),
             " is required to determine neighbourhood orders")

    stopifnot(isScalar(maxlag), maxlag > 0)
    checkNeighbourhood(neighbourhood)
    neighbourhood <- neighbourhood == 1           # convert to binary matrix
    nregions <- nrow(neighbourhood)
    maxlag <- as.integer(min(maxlag, nregions-1)) # upper bound of nb order
    
    if (maxlag == 1L) {
        storage.mode(neighbourhood) <- "integer"
        return(neighbourhood)
    }

    ## manually convert to spdep's "nb" class
    ## region.idxs <- seq_len(nregions)
    ## nb <- lapply(region.idxs, function(i) {
    ##     nbs <- which(neighbourhood[i,])
    ##     if (length(nbs) > 0L) nbs else 0L
    ## })
    ## class(nb) <- "nb"

    ## convert first-order neighbourhood to spdep's "nb" class
    nb <- spdep::mat2listw(neighbourhood)$neighbours
    attr(nb, "region.id") <- NULL

    ## compute higher order neighbours using spdep::nblag()
    nb.lags <- spdep::nblag(nb, maxlag=maxlag)

    ## Side note: fast method to determine neighbours _up to_ specific order:
    ## crossprod(neighbourhoud) > 0  # up to second order neighbours (+set diag to 0)
    ## (neighbourhood %*% neighbourhood %*% neighbourhood) > 0  # up to order 3
    ## and so on...

    ## convert to a single matrix
    nbmat <- neighbourhood   # logical first-order matrix
    storage.mode(nbmat) <- "numeric"
    for (lag in 2:maxlag) {
        if (any(spdep::card(nb.lags[[lag]]) > 0L)) { # any neighbours of this order
            nbmat.lag <- spdep::nb2mat(nb.lags[[lag]], style="B",
                                       zero.policy=TRUE)
            nbmat <- nbmat + lag * nbmat.lag
        }
    }
    attr(nbmat, "call") <- NULL
    storage.mode(nbmat) <- "integer"

    ## message about maximum neighbour order by region
    maxlagbyrow <- apply(nbmat, 1, max)
    message("Note: range of maximum neighbour order by region is ",
            paste(range(maxlagbyrow), collapse="-"))

    ## Done
    nbmat
}


### Internal wrapper for maptools::unionSpatialPolygons

unionSpatialPolygons <- function (SpP)
{
    if (require("maptools") && maptools::gpclibPermitStatus()) {
        W <- maptools::unionSpatialPolygons(
            SpP, IDs = rep.int(1,length(SpP@polygons)), avoidGEOS = TRUE)
        ## ensure that W has exactly the same proj4string as SpP
        ## since the internal CRS()-call might have modified it
        W@proj4string <- SpP@proj4string
        W
    } else {
        stop("package \"maptools\" required, and \"gpclibPermit()\" therein")
    }
}

    
### determines which polygons of a SpatialPolygons object are at the border,
### i.e. have coordinates in common with the spatial union of all polygons

polyAtBorder <- function (SpP)
{
    W <- unionSpatialPolygons(SpP)
    Wcoords <- unique(coordinates(W@polygons[[1]]@Polygons[[1]]))
    atBorder <- sapply(SpP@polygons, function (x) {
        coords <- coordinates(x@Polygons[[1]])
        anyDuplicated(rbind(unique(coords), Wcoords)) > 0
    })
    names(atBorder) <- row.names(SpP)
    atBorder
}

################################################################################
### Some helper functions for 'twinstim'.
###
### Author: Sebastian Meyer
### $Date: 2010-11-16 04:08:22 +0100 (Tue, 16 Nov 2010) $
################################################################################


### Method for coercion from "Polygons" (sp) to "gpc.poly" (gpclib) and vice versa

# Varying polygon specifications in the packages:
# sp: REPEAT first vertex at the end (closed), anticlockwise = hole, clockwise = normal boundary
# spatstat: do NOT REPEAT first vertex, anticlockwise = normal boundary, clockwise = hole
# gpc.poly: NOT DOCUMENTED, but it seems that it is prefered to not repeat the first vertex and to have clockwise vertex order for the normal boundary. I thus opt for the "sp" convention!
setAs(from = "Polygons", to = "gpc.poly", def = function (from)
    {
        pls <- slot(from, "Polygons")
        pts <- lapply(pls, function (sr) {
            coords <- coordinates(sr)
            n <- nrow(coords) - 1   # number of vertices
            list(x = coords[seq_len(n),1],
                 y = coords[seq_len(n),2],
                 hole = sr@hole)
        })
        new("gpc.poly", pts = pts)
    }
)

setAs(from = "gpc.poly", to = "Polygons", def = function (from)
    {
        srl <- lapply(get.pts(from), function (poly) {
            if (isClosed(poly)) {
                Polygon(cbind(poly$x,poly$y), hole = poly$hole)
            } else {
                Polygon(cbind(c(poly$x,poly$x[1]), c(poly$y,poly$y[1])),
                        hole = poly$hole)
            }
        })
        Polygons(srl, ID = "1")
    }
)



### Method for coercion from "SpatialPolygons" (sp) to "gpc.poly" (gpclib)
### This method also applies to "SpatialPolygonsDataFrame" (inherited)

setAs(from = "SpatialPolygons", to = "gpc.poly", def = function (from)
    {
        polygonsList <- polygons(from)@polygons
        gpc <- new("gpc.poly")
        for (i in seq_along(polygonsList))
        {
            gpc <- append.poly(gpc, as(polygonsList[[i]], "gpc.poly"))
        }
        gpc
    }
)



### Method for coercion from "owin" (spatstat) to "gpc.poly" (gpclib)

#setOldClass("owin")    # does not work when package "maptools" is loaded, don't fully understand this matter...
setAs(from = "owin", to = "gpc.poly", def = function (from)
    {
        pts <- lapply(from$bdry, function (poly) {
            list(x = rev(poly$x), y = rev(poly$y), hole = poly$hole)
        })
        new("gpc.poly", pts = pts)
    }
)



### Expansion of the gpclib::scale.poly function to also do centering

# removeMethod("scale.poly", signature(x = "gpc.poly"))
# setMethod("scale.poly", signature(x = "gpc.poly"),
# Redefining the S4 method from gpclib did not work
# (don't know how to get this right)... so I define a S3 method for scale
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

inside.gpc.poly <- function(x, y, polyregion, mode.checked = FALSE)
{
    stopifnot(length(y) == (N <- length(x)))
    # check for each polygon of polyregion if points are in the polygon
    locations <- sapply(get.pts(polyregion), function(poly) {
        pip <- point.in.polygon(x, y, poly$x, poly$y, mode.checked = mode.checked)
        if (poly$hole) { # if point is inside a hole then attribute -Inf
            ifelse(pip == 1, -Inf, 0)
        } else pip
    })
    inside <- if (N == 1L) sum(locations) > 0 else rowSums(locations) > 0
    return(inside)
}



### Checks if the first and last coordinates of a coordinate matrix are equal

isClosed <- function (coords)
{
    xycoords <- xy.coords(coords)[c("x","y")]
    n <- length(xycoords$x)
    return(identical(xycoords$x[1], xycoords$x[n]) &&
           identical(xycoords$y[1], xycoords$y[n]))
}



## Returns a Polygon representing a disc (in planar coordinates)

# center: center of the disc
# r: radius in km
# npoly: Number of edges of the polygonal approximation
# hole: hole flag of the polygon
discpoly <- function (center, r, npoly = 64,
    class = c("gpc.poly", "owin", "Polygon"), hole = FALSE)
{
    class <- match.arg(class)
    if (class == "owin") {
        res <- disc(radius = r, center = center, mask = FALSE, npoly = npoly)   # spatstat::disc
        if (hole) {
            res$bdry[[1]]$x <- rev(res$bdry[[1]]$x)
            res$bdry[[1]]$y <- rev(res$bdry[[1]]$y)
            res$bdry[[1]]$hole <- TRUE
        }
        return(res)
    }

    theta <- seq(2*pi, 0, length = npoly+1)[-(npoly+1)]   # for clockwise order
    if (hole) theta <- rev(theta)   # for anticlockwise order
    x <- center[1] + r * cos(theta)
    y <- center[2] + r * sin(theta)
    switch(class,
        "Polygon" = Polygon(cbind(c(x,x[1]),c(y,y[1])), hole=hole),
        "gpc.poly" = new("gpc.poly", pts = list(list(x=x, y=y, hole=hole)))
    )
}



## Compute the intersection of a "gpc.poly" with a "discpoly" (and centering)

intersectCircle <- function (Wgpc, center, r, npoly)
{
    circle <- discpoly(center = center, r = r, npoly = npoly,
                       class = "gpc.poly", hole = FALSE)
    intersection <- intersect(circle, Wgpc)  # this order seems to be faster
    scale(intersection, center = center)  # -> scale.gpc.poly defined above
}



## Compute distance from points to boundary
## (adapted from spatstat::bdist.points, DEPENDS ON spatstat::distppl)

# xy is the coordinate matrix of the points
# poly is a polygonal domain of class "gpc.poly"
# the function does not check if points are actually inside the polygonal domain
bdist <- function (xy, poly)
{
    result <- rep(Inf, nrow(xy))
    bdry <- poly@pts
    for (i in seq_along(bdry)) {
        polly <- bdry[[i]]
        nsegs <- length(polly$x)
        for (j in 1L:nsegs) {
            j1 <- if (j < nsegs) j + 1L else 1L
            seg <- c(polly$x[j], polly$y[j], polly$x[j1], polly$y[j1])
            result <- pmin(result, distppl(xy, seg))
        }
    }
    return(result)
}



### Determines indexes of potential sources of infection of event i

# i only indexes eventTimes, which is an nEvents-vector like all other arguments
determineSources <- function (i, eventTimes, removalTimes, distvec, eps.s,
    eventTypes = NULL, qmatrix)
{
    tp <- eventTimes[i]
    type <- eventTypes[i]   # NULL[i] -> NULL
    infectivity <- (eventTimes < tp) & (removalTimes >= tp)
    #<- eventTimes<tp, not "=" because CIF is left-continuous. Also guarantees no self-infection
    proximity <- as.vector(distvec <= eps.s, mode = "logical")
    #<- as.vector to remove names
    matchType <- if (is.null(eventTypes)) TRUE else {
        typeInfective <- qmatrix[,type]   # indexing by internal integer code of factor
        #<- logical vector indicating for each type if it could infect type of i
        as.vector(typeInfective, mode = "logical")[eventTypes]
        #<- as.vector to remove names
    }
    # return IDs of potential epidemic sources
    which(infectivity & proximity & matchType)
}



### Check matrix Q

checkQ <- function (qmatrix, typeNames)
{
    nTypes <- length(typeNames)
    qmatrix <- as.matrix(qmatrix)
    stopifnot(nrow(qmatrix) == ncol(qmatrix))
    if (is.null(dimnames(qmatrix))) {
        if (nrow(qmatrix) != nTypes) {
            stop("'qmatrix' must be a ", nTypes, "x", nTypes, " matrix")
        }
        dimnames(qmatrix) <- list(typeNames, typeNames)
    } else {
        stopifnot(rownames(qmatrix) == colnames(qmatrix))
        typesIdx <- match(typeNames, rownames(qmatrix), nomatch=NA_integer_)
        if (idx <- match(TRUE, is.na(typesIdx), nomatch=0L)) {
            stop("missing entries for type '", typeNames[idx], "' in 'qmatrix'")
        }
        qmatrix <- qmatrix[typesIdx,typesIdx,drop=FALSE]
    }
    storage.mode(qmatrix) <- "logical"   # convert entries to TRUE/FALSE by as.logical
    qmatrix
}


### Get row index of 'stgrid' where an event is located (spatio-temporally)
# here, search BLOCK such that t in (start;stop], i.e. an event at 'stop' is
# still attributed to the previous interval

gridcellOfEvent <- function (t, tilename, stgrid)
{
    idx <- with(stgrid, which(tile == tilename & start < t & stop >= t))
    # faster alternative for a one-length factor 'tile', but the above using 'tilename' is less error-prone...
    #idx <- which(stgrid$start < tp & stgrid$stop >= tp)[tile]   # index by numeric code of 'tile'
    lidx <- length(idx)
    if (lidx == 0L) NA_integer_ else if (lidx == 1L) idx else {
        stop("'stgrid' has overlapping spatio-temporal grid cells")
    }
}

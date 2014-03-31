################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[uselessSimplificationIn.R] by SM Mon 31/03/2014 17:15 (CEST)>
### Project: polygon simplification using R packages
################################################################################

library("sp")
load("kreise.RData")
plot(kreise)


## the tools provided by rgeos, maptools, or spatstat all treat the polygons
## individually, i.e. don't keep them connected at the edge

library("rgeos")
d1 <- gSimplify(kreise, tol=10, topologyPreserve=TRUE)
plot(d1)

library("maptools")
d2 <- thinnedSpatialPoly(kreise, 10, avoidGEOS=TRUE)
plot(d2)


library("spatstat")
o1 <- maptools::as.owin.SpatialPolygons(kreise)

library("polyCub")
spatstat.options(fixpolygons=FALSE)     # otherwise joins the districts!
o2 <- as(kreise, "owin")
o2$bdry <- lapply(o2$bdry, function (poly) {
    L <- length(poly$x)
    perm <- c(L, seq_len(L-1))
    poly$x <- poly$x[perm]
    poly$y <- poly$y[perm]
    poly[c("x", "y")]
})
stopifnot(all.equal(o1, o2, check.attributes=FALSE))
plot(simplify.owin(o2, 10))


## => pass via mapshaper.org (see kreiseSimple.R)

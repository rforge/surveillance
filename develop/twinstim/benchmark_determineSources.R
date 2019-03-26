## performance checks for new C++ implementation of determineSources()

##devtools::load_all("~/Projekte/surveillance/pkg")
library("surveillance")

### runtime comparison using imdepi data

Rcpp::sourceCpp("~/Projekte/surveillance/pkg/src/determineSources.cc", embeddedR = TRUE)
## surveillance 1.17.0 in R 3.5.3:
## C: 17.9 ms
## R: 59.3 ms


### runtime comparison using large PUK data

load("~/Projekte/PUK/data/pukepi_all.RData")
object <- pukepi_all

system.time(a <- surveillance:::determineSources.epidataCS(object, method = "R"))
## takes around 18 seconds

system.time(b <- surveillance:::determineSources.epidataCS(object, method = "C"))
## takes around 6 seconds

stopifnot(identical(a, b))

## run <- function (FUN = determineSourcesC)
##     FUN(eventTimes = object$events$time, eps_t = object$events$eps.t,
##         eventCoords = object$events@coords, eps_s = object$events$eps.s,
##         eventTypes = as.integer(object$events$type), qmatrix = object$qmatrix)
## system.time(b <- run())

## eventCoords <- object$events@coords
## microbenchmark(distsN1(eventCoords[,1], eventCoords[,2], eventCoords[1,1], eventCoords[1,2]),
##                times = 500)


### profiling whole as.epidataCS() procedure

Rprof("/tmp/Rprof.out")
epi <- as.epidataCS(
    events = SpatialPointsDataFrame(coordinates(object$events),
                                    marks(object, coords=FALSE),
                                    proj4string = object$events@proj4string),
    stgrid = object$stgrid[,-1L], W = object$W, qmatrix = object$qmatrix,
    nCircle2Poly = 16, clipper = "polyclip")
Rprof(NULL)
summaryRprof("/tmp/Rprof.out")

library("surveillance")

### Gaussian kernel
checks.gaussian <- surveillance:::checksiaf(
    siaf.gaussian(logsd=TRUE, F.adaptive=TRUE),
    pargrid = log(1),
    tolerance = 0.01)
stopifnot(unlist(lapply(checks.gaussian, attr, "all.equal")))

### Power-law kernel
checks.powerlaw <- surveillance:::checksiaf(
    siaf.powerlaw(),
    pargrid = t(c(0.5, -0.5)),
    tolerance = 0.0005)
stopifnot(unlist(lapply(checks.powerlaw, attr, "all.equal")))

### Lagged power law
checks.powerlawL <- surveillance:::checksiaf(
    siaf.powerlawL(),
    pargrid = t(c(-0.5, -0.5)),
    tolerance = 0.0005)
stopifnot(unlist(lapply(checks.powerlawL, attr, "all.equal")))

### Student t-kernel
checks.student <- surveillance:::checksiaf(
    siaf.student(),
    pargrid = t(c(0.5, -0.5)),
    tolerance = 1e-6)
stopifnot(unlist(lapply(checks.student, attr, "all.equal")))

### Step function kernel
spatstat::spatstat.options(npixel=600)
checks.step <- surveillance:::checksiaf(
    siaf.step(c(0.1,0.5,1)),
    pargrid = -cbind(0.5,0.1,0.2),
    method = "midpoint",
    tolerance = 1e-3)
stopifnot(unlist(lapply(checks.step, attr, "all.equal")))


## showsiaf <- function (siaf, pars) {
##     data("letterR", package="spatstat", envir=environment())
##     poly <- spatstat::shift.owin(letterR, -c(3,2))
##     plotpolyf(poly, siaf$f, pars, print.args=list(split=c(1,1,2,1), more=TRUE))
##     plotpolyf(poly, function (...) siaf$deriv(...)[,1], pars, print.args=list(split=c(2,1,2,1)))
## }
## showsiaf(siaf.student(), c(0.5,-0.5))

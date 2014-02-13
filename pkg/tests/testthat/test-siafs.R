context("Spatial interaction functions")

myexpectation <- function (siaf, pars, ...) {
    checksiafres <- surveillance:::checksiaf(siaf, t(pars), ...)
    for (i in which(!sapply(checksiafres, is.null)))
        expect_true(unique(attr(checksiafres[[i]], "all.equal")),
                    label=paste0(names(checksiafres)[i], " for pars=",
                    deparse(substitute(pars))))
}

test_that("Gaussian implementation agrees with numerical approximation",
          myexpectation(siaf.gaussian(logsd=TRUE, F.adaptive=TRUE),
                        pars=log(1), tolerance=0.01))

test_that("Power-law implementation agrees with numerical approximation",
          myexpectation(siaf.powerlaw(), pars = c(0.5,-0.5), tolerance=0.0005))

test_that("Lagged power-law implementation agrees with numerical results",
          myexpectation(siaf.powerlawL(), pars=c(-0.5,-0.5), tolerance=0.0005))

test_that("Student (t) implementation agrees with numerical approximation",
          myexpectation(siaf.student(), pars=c(0.5,-0.5), tolerance=1e-6))

test_that("Step kernel implementation agrees with numerical approximation", {
    spatstat::spatstat.options(npixel=350)
    myexpectation(siaf.step(c(0.1,0.5,1)), pars=-c(0.5,0.1,0.2),
                  method="midpoint", tolerance=0.0005)
})


### plot the polygon on which F and Deriv are tested:
## showsiaf <- function (siaf, pars) {
##     data("letterR", package="spatstat", envir=environment())
##     poly <- spatstat::shift.owin(letterR, -c(3,2))
##     plotpolyf(poly, siaf$f, pars, print.args=list(split=c(1,1,2,1), more=TRUE))
##     plotpolyf(poly, function (...) siaf$deriv(...)[,1], pars, print.args=list(split=c(2,1,2,1)))
## }
## showsiaf(siaf.student(), c(0.5,-0.5))

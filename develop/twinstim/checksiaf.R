################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[checksiaf.R] by SM Fre 20/12/2013 09:57 (CET)>
### Description: Check new spatial interaction function
################################################################################

siaf <- siaf.student()

## grid of parameters
pargrid <- as.matrix(expand.grid(
    sigma=c(0.2,0.5,1,1.5,2,4),
    d=c(0.2,0.5,1,1.5,2,4)
    ))

## standard checks
surveillance:::checksiaf(siaf, pargrid)


### specifically check intrfr's

## integration domain
data("letterR", package="spatstat")
poly <- spatstat::shift.owin(letterR, -c(3,2))

## plot functions for first parameter set
polyCub.iso(poly, siaf$f,
            function(R, logpars)
            surveillance:::intrfr.student(R, exp(logpars[[1]]), exp(logpars[[2]])),
            pargrid[1,], center=c(0,0), plot=TRUE)

polyCub.iso(poly, function(...) siaf$deriv(...)[,1],
            function(R, logpars)
            surveillance:::intrfr.student.dlogsigma(R, exp(logpars[[1]]), exp(logpars[[2]])),
            pargrid[1,], center=c(0,0), plot=TRUE)

polyCub.iso(poly, function(...) siaf$deriv(...)[,2],
            function(R, logpars)
            surveillance:::intrfr.student.dlogd(R, exp(logpars[[1]]), exp(logpars[[2]])),
            pargrid[1,], center=c(0,0), plot=TRUE)


## check intrfr.student
for (i in 1:nrow(pargrid)) {
    cat(i, ": ")
    polyCub:::checkintrfr(
        function (R, logpars)
        surveillance:::intrfr.student(R, exp(logpars[[1]]), exp(logpars[[2]])),
        siaf$f,
        pargrid[i,], center=c(0,0), rs=c(0.1,0.5,1,2))
}

## check intrfr.student.dlogsigma
for (i in 1:nrow(pargrid)) {
    cat(i, ": ")
    polyCub:::checkintrfr(
        function (R, logpars)
        surveillance:::intrfr.student.dlogsigma(R, exp(logpars[[1]]), exp(logpars[[2]])),
        function(...) siaf$deriv(...)[,1],
        pargrid[i,], center=c(0,0), rs=c(0.1,0.5,1,2))
}

## check intrfr.student.dlogd
for (i in 1:nrow(pargrid)) {
    cat(i, ": ")
    polyCub:::checkintrfr(
        function (R, logpars)
        surveillance:::intrfr.student.dlogd(R, exp(logpars[[1]]), exp(logpars[[2]])),
        function(...) siaf$deriv(...)[,2],
        pargrid[i,], center=c(0,0), rs=c(0.1,0.5,1,2))
}

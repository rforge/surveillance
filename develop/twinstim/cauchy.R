################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[cauchy.R] by SM Don 05/09/2013 21:24 (CEST)>
### Project: try cauchy-family parametrization of power-law kernel
################################################################################


library("surveillance")
data("imdepi")
data("imdepifit")

## break tied locations
set.seed(5913)
imdepi_untied <- untie(imdepi, amount=list(s=NULL))


### fit with Lomax kernel

m0 <- update(imdepifit,
             epidemic = ~1, siaf = siaf.powerlaw(),
             data = imdepi_untied, subset = type=="B",
             control.siaf = list(F=list(nGQ=30,adapt=NULL), Deriv=list(nGQ=30)),
             start = c("e.(Intercept)"=-9, "e.siaf.1" = -0.5, "e.siaf.2" = 0.5),
             model=TRUE, cumCIF = FALSE, cores=4
             )


### fit with cauchy family parametrization of the power law

siaf.cauchy <- list(
    f = function (s, logpars) {
        logsigma <- logpars[[1L]]
        logd <- logpars[[2L]]
        sigma <- exp(logsigma)
        d <- exp(logd)
        sLength <- sqrt(.rowSums(s^2, nrow(s), 2L))
        (1 + sLength/sigma)^-d          # = siaf.powerlaw()$f / sigma^-d
    },
    npars = 2
    )

gamma00 <- coef(m0)["e.(Intercept)"]
sigma <- exp(coef(m0)["e.siaf.1"])
d <- exp(coef(m0)["e.siaf.2"])
gamma01 <- gamma00 -d*log(sigma)

m1 <- update(m0, siaf = siaf.cauchy, start = c(gamma01))
m1$fisherinfo.observed <- -optimHess(coef(m1), m1$functions$ll,
                                     control=list(fnscale=-1))

## compare coefficients
cbind(coef(m0), coef(m1))               # identical except for e.(Intercept)

## compare standard errors
cbind(sqrt(diag(vcov(m0))), sqrt(diag(vcov(m1))))
## for Lomax kernel, sigma and gamma0 are more precise

## compare correlations
round(unname(summary(m0, correlation=TRUE)$correlation), 2)
round(unname(summary(m1, correlation=TRUE)$correlation), 2)
## slightly less correlation of siafpars with e.intercept


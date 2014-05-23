################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[cauchy.R] by SM Fre 23/05/2014 14:09 (CEST)>
### Project: try cauchy-family parametrization of power-law kernel
################################################################################


library("surveillance")
data("imdepi")
data("imdepifit")

## break tied locations
set.seed(321)
imdepi_untied <- untie(imdepi, amount=list(s=NULL))


### fit with power-law kernel

m0 <- update(imdepifit,
             epidemic = ~1, siaf = siaf.powerlaw(),
             data = imdepi_untied, subset = type=="B",
             control.siaf = list(F=list(), Deriv=list()),
             start = c("e.(Intercept)"=-9, "e.siaf.1" = -2, "e.siaf.2" = 0.5),
             model=TRUE, cumCIF = FALSE, cores=4
             )


### fit with cauchy family parametrization of the power law

siaf.cauchy <- list(f=siaf.powerlaw()$f, npars=2)
body(siaf.cauchy$f)[[length(body(siaf.cauchy$f))]] <-
    quote((sLength/sigma + 1)^-d)  # = siaf.powerlaw()$f / sigma^-d

gamma00 <- coef(m0)["e.(Intercept)"]
sigma <- exp(coef(m0)["e.siaf.1"])
d <- exp(coef(m0)["e.siaf.2"])
gamma01 <- gamma00 -d*log(sigma)

m1 <- update(m0, siaf = siaf.cauchy, start = c(gamma01),
             control.siaf = list(F=list(nGQ=45)))
## => same model, no change in parameters (except adjusted e.(Intercept))
cbind(coef(m0), coef(m1))

## without providing deriv in siaf.cauchy, we have no score/expected fisherinfo
## use numerical approximation of observed fisherinfo
m1$fisherinfo.observed <- -optimHess(coef(m1), m1$functions$ll,
                                     control=list(fnscale=-1))
## => also for m0 for comparability
m0$fisherinfo.observed <- -optimHess(coef(m0), m0$functions$ll,
                                     control=list(fnscale=-1))
## NOTE: some off-diagonal elements are quiet different from the expected fisherinfo
unname(m0$fisherinfo / m0$fisherinfo.observed)
## especially the elements for sin/cos, trend/cos, sigma/d
m0["fisherinfo"] <- list(NULL)  # remove for same use of vcov below

## compare standard errors
cbind(sqrt(diag(vcov(m0))), sqrt(diag(vcov(m1))))
## sigma and d are slightly more precise with Lomax than with Cauchy parametrization

## compare correlations
round(unname(summary(m0, correlation=TRUE)$correlation), 2)
round(unname(summary(m1, correlation=TRUE)$correlation), 2)
## cauchy (m1): strong negative correlation of sigma with e.intercept (-0.97)
## Lomax (m0): positive correlations of siafpars with e.intercept (about 0.9)

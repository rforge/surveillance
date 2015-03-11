################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[fithom.R] 2015-03-11 18:32 (CET) by SM>
###
### simulate independent events with spatio-temporal homogeneous intensity
### and fit epidemic models where there is no space-time clustering
### => parametric interaction functions diverge to point mass in 0
################################################################################

library("surveillance")
load(system.file("shapes", "districtsD.RData", package="surveillance"))

## simulate homogeneous "epidataCS"
set.seed(11315)
simhom <- simEpidataCS(
    endemic = ~1, rmarks = function (...) data.frame(eps.t = Inf, eps.s = Inf),
    stgrid = data.frame(start=0, stop=2500, tile="D", area=stateD@polygons[[1]]@area),
    tiles = stateD, W = stateD, beta0 = -14, trace = 100
)

## fit the correct model
fithom <- twinstim(endemic = ~1, data = simhom, model = TRUE)

## add a simple epidemic component (infdelta, infeps)
fitepi1 <- update(fithom, epidemic = ~1)
plot(fitepi1, "epidemic")
summary(R0(fitepi1)) # 0.01
logLik(fithom)
logLik(fitepi1)

## assume _locally_ constant infectivity
fitepi1local <- update(fitepi1,
                       siaf = siaf.step(numeric(0), maxRange=50),
                       tiaf = tiaf.step(numeric(0), maxRange=30))
## -> singular convergence reported, epidemic component -> 0
summary(fitepi1local) # -> log(epidemic intercept) is -30 with enormous SE
summary(R0(fitepi1local)) # essentially 0
c(logLik(fithom), logLik(fitepi1local)) # equivalent models


### in the following we leave eps.t = Inf alone and try to fit the 'siaf'

## fit a spatial step function
fitstep <- update(fitepi1, siaf = siaf.step(c(5, 10, 25), maxRange = 50))
## -> singular convergence
diag(MASS::ginv(fitstep$fisherinfo))
plot(fitstep, "siaf", conf.type = "none", xlim=c(0,200), scaled=FALSE)
## -> towards a pole at 0!
c(logLik(fithom), logLik(fitepi1), logLik(fitstep))
summary(R0(fitstep)) # 0.005

## fit a Gaussian kernel
## fithomGauss <- update(fitepi1, cores = 3,
##                       siaf = siaf.gaussian(effRangeMult = 3),
##                       start = c("e.siaf.1" = 1),
##                       control.siaf = list(F=list(adapt=0.25)))
## logsigma becomes smaller and smaller until adaptive polyCub.midpoint
## no longer has enough memory... last value sigma = exp(-0.62) = 0.54 km ...
## -> use polyCub.SV
fitGauss <- update(fitepi1, cores = 3,
                   siaf = siaf.gaussian(F.adaptive = FALSE),
                   start = c("e.siaf.1" = -0.62))
## -> singular convergence, e.siaf.1 ~ -1
summary(fitGauss) # huge SE of epidemic parameters
plot(fitGauss, "siaf", conf.type = "none", xlim=c(0,200))
## -> towards a pole at 0!
c(logLik(fithom), logLik(fitepi1), logLik(fitstep), logLik(fitGauss))
plot(fitGauss, "epidemic")
summary(R0(fitGauss)) # essentially 0 -> equivalent to fithom

## fit a powerlaw (TAKES AGES!!!)
##fitPL <- update(fitepi1, siaf = siaf.powerlaw())
## I stopped optimization at
## 42:     11451.730: -13.9871 -11.2925  5.01102 0.757313
fitPL <- update(fitepi1, siaf = siaf.powerlaw(),
                start = c("e.siaf.1" = 5.0, "e.siaf.2" = 0.76),
                optim.args = list(fixed = c("e.siaf.1", "e.siaf.2")))
## -> singular convergence
fitPL$fisherinfo  # flat likelihood for epidemic parameters...
plot(fitPL, "siaf", conf.type = "none", xlim=c(0,200), scaled = FALSE)
## -> no pole at 0, seems not yet converged, but anyway
summary(R0(fitPL)) # essentially 0
plot(fitPL, "epidemic")
c(end = fithom$loglik, PL = fitPL$loglik) # equivalent

## pseudo-constant powerlaw (very low d)
fitPLconst <- update(fitepi1, siaf = siaf.powerlaw(),
                     start = c("e.siaf.1" = 0, "e.siaf.2" = -5),
                     optim.args = list(fixed = c("e.siaf.1", "e.siaf.2")))
plot(fitPLconst, "siaf", conf.type = "none", xlim=c(0,200))
summary(R0(fitPLconst)) # 0.01 (similar to fitepi1, of course)

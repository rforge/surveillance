################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[checkfisher.R] by SM Die 23/04/2013 22:56 (CEST)>
### Project: Check analytical expected Fisherinfo by numerical observed
###          Fisherinfo (obtained from optimHess)
################################################################################

library("surveillance")
surveillance.options(gpclib=TRUE)
data("imdepi")
data("imdepifit")
fit <- update(imdepifit, model=TRUE)


##########################
### check with optimHess()
##########################

## compute numerical approximation of the observed Fisher Information
hess <- optimHess(coef(fit), fit$functions$ll, gr=fit$functions$sc,
                  control=list(fnscale=-1))
iobs <- -hess

## compare with analytical expected Fisherinfo
reldiff <- iobs / fit$fisherinfo - 1
unname(round(reldiff,2))

library("Matrix")
image(Matrix(reldiff))

## huge differences in cells linked to the siaf parameter (related to numerical
## cubature?)


####################################################
### check vcov via parametric bootstrap (simulation)
####################################################

## simpler model and reduced dataset
smallfit <- update(fit, epidemic=~.-agegrp, t0 = 365, T = 730,
                   model=FALSE, cumCIF=FALSE)

## simulate & fit
if (FALSE) {
    B <- 200
    load(system.file("shapes", "districtsD.RData", package="surveillance"))
    sims <- simulate(smallfit, nsim=B, seed=1, data=imdepi,
                     tiles=districtsD, W=stateD, trace=FALSE,
                     .allocate=150, simplify=TRUE)
    
    fitsims <- vector(mode="list", length=B)
    for (i in 1:B) {
        cat("\nFitting simulated epidemic", i, ":\n")
        fitsims[[i]] <- update(smallfit, data=sims[[i]])
    }

    save(sims, fitsims, file="checkfisher.RData")
    tools::resaveRdaFiles("checkfisher.RData")
} else {
    load("checkfisher.RData")
}

bootcoefs <- t(sapply(fitsims, coef))
bootOK <- sapply(fitsims, "[[", "converged")
table(bootOK)  # 57 did not converge
boxplot(bootcoefs)
boxplot(bootcoefs[bootOK,])
bootcov <- cov(bootcoefs)
bootcovOK <- cov(bootcoefs[bootOK,])

unname(round(bootcov,2))
unname(round(bootcovOK,3))
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]
## [1,]  1.175 -0.791 -0.254 -0.116  0.063 -0.064 -0.041
## [2,] -0.791  0.540  0.171  0.074 -0.037  0.036  0.022
## [3,] -0.254  0.171  0.090  0.031 -0.016  0.006  0.008
## [4,] -0.116  0.074  0.031  0.045 -0.001 -0.002  0.004
## [5,]  0.063 -0.037 -0.016 -0.001  0.298 -0.173 -0.095
## [6,] -0.064  0.036  0.006 -0.002 -0.173  0.564  0.011
## [7,] -0.041  0.022  0.008  0.004 -0.095  0.011  0.063
unname(round(vcov(smallfit),3))
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]
## [1,]  0.911 -0.620 -0.186 -0.086 -0.028  0.010  0.011
## [2,] -0.620  0.430  0.127  0.057  0.018 -0.009 -0.009
## [3,] -0.186  0.127  0.068  0.015 -0.001  0.006 -0.002
## [4,] -0.086  0.057  0.015  0.041  0.002  0.003 -0.005
## [5,] -0.028  0.018 -0.001  0.002  0.220 -0.130 -0.054
## [6,]  0.010 -0.009  0.006  0.003 -0.130  0.352 -0.005
## [7,]  0.011 -0.009 -0.002 -0.005 -0.054 -0.005  0.037

reldiff <- vcov(smallfit) / bootcovOK - 1
library("Matrix")
image(Matrix(reldiff))
## quiet large differences, but the order of magnitude is correct
## would probably need a larger sample
## however, the fisherinfo in twinstim is also an approximation only

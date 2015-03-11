################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[permutation.R] 2015-03-11 18:42 (CET) by SM>
###
### permutation test to evaluate endemic vs. endemic+epidemic
################################################################################

library("surveillance")
data("imdepi")
data("imdepifit")
load(system.file("shapes/districtsD.RData", package = "surveillance"))
t0 <- 1826  # to make this faster
NPERM <- 50

## compare models in the observed imdepi
mend <- twinstim(endemic = addSeason2formula(~1, period=365, timevar="start"),
                 data = imdepi, t0 = t0)
m1 <- update(mend, epidemic = ~1, start = c("e.(Intercept)" = -15))
AIC(mend, m1)
## observed likelihood ratio statistic
(myD <- 2 * (m1$loglik - mend$loglik))  # 28.9


###############################################
### 1. using simulations from the endemic model
###############################################

## simulate from the endemic model
mendsims <- simulate(mend, nsim=NPERM, seed=1, data = imdepi, tiles = districtsD)

## calculate the likelihood ratio in all simulations
D0sim <- lapply(seq_along(mendsims$eventsList), function (i) {
    cat(".")
    m0 <- update(mend, data = mendsims[[i]], model = FALSE, cumCIF = FALSE,
                 verbose = FALSE, optim.args = list(control = list(trace = 0)))
    m1 <- update(m0, epidemic = ~1, start = c("e.(Intercept)" = -15))
    list(D = 2 * (m1$loglik - m0$loglik),
         converged = isTRUE(m0$converged) & isTRUE(m1$converged),
         m0 = m0, m1 = m1)
})

(idxconverged <- which(sapply(D0sim, "[[", "converged")))
i = 1
plot(update(D0sim[[i]]$m1, model=TRUE), "epidemic")
i = 2
plot(update(D0sim[[i]]$m1, model=TRUE), "epidemic")

sapply(D0sim[idxconverged], function(x) coef(x$m1)["e.(Intercept)"])
sapply(D0sim[-idxconverged], function(x) coef(x$m1)["e.(Intercept)"])
## in the non-convergent fits the epidemic intercept is too small (numerically)

## distribution of the likelihood ratio statistic
D0simdist <- sapply(D0sim[idxconverged], "[[", "D")
boxplot(D0simdist, ylim = range(D0simdist, myD)); points(myD, pch=4)
## observed D is far out (p=0)


###############################################################
### 2. permutation test via relabeling locations or time points
###############################################################

getD <- function (endemicdata, verbose = TRUE)
{
    if (verbose) cat(".")
    m0 <- update(mend, data = endemicdata, model = FALSE, cumCIF = FALSE,
                 verbose = FALSE, optim.args = list(control = list(trace = 0)))
    m1 <- update(m0, epidemic = ~1, start = c("e.(Intercept)" = -15))
    list(D = 2 * (m1$loglik - m0$loglik),
         converged = isTRUE(m0$converged) & isTRUE(m1$converged),
         m0 = m0, m1 = m1)
}


### a) permute imdepi locations (keeping the pre-history unchanged)

set.seed(1)
imdepiperms <- replicate(NPERM, permute.epidataCS(imdepi, "space", keep = time <= t0), simplify = FALSE)

D0perms <- lapply(imdepiperms, getD)

## epidemic intercept
summary(sapply(D0perms, function(x) coef(x$m1)["e.(Intercept)"]))

## distribution of the likelihood ratio statistic
D0permsdist <- sapply(D0perms, "[[", "D")
MASS::truehist(D0permsdist, xlim = range(D0permsdist, myD)); abline(v=myD, lwd=2)
## observed D is far out (p=0)


### b) permute imdepi time points

set.seed(1)
imdepipermt <- replicate(NPERM, permute.epidataCS(imdepi, "time", keep = time <= t0), simplify = FALSE)

D0permt <- lapply(imdepipermt, getD)

## epidemic intercept
summary(sapply(D0permt, function(x) coef(x$m1)["e.(Intercept)"]))

## distribution of the likelihood ratio statistic
D0permtdist <- sapply(D0permt, "[[", "D")
MASS::truehist(D0permtdist, xlim = range(D0permtdist, myD)); abline(v=myD, lwd=2)
## observed D is much closer to the permutation distribution
mean(c(D0permtdist, myD) >= myD) # 0.04


### why is the distribution under the null
### different for time / space relabeling?

## permuting time labels gives a more conservative result here

## permutation test originally proposed by mantel1967
## besag.diggle1977 "random permutations of space or time labels"

## time permutation has also been used by
## - diggle.etal1995
## - kulldorff.hjalmars1999
## - rogerson2001 (mentions location-permutation as an alternative)
## - jacquez1996

## location permutation has been used by
## - gabriel.diggle2009

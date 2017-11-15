################################################################################
### Author: Sebastian Meyer [seb *.* meyer *a*t* fau *.* de]
### Time-stamp: <[SEI.R] 2017-11-15 14:22 (CET) by SM>
### Project: Bug report on tE.col by Caterina de Bacco (e-mail, 2017-11-14)
###
### In surveillance <= 1.15.0, specifying latent periods ('tE.col') was actually
### not possible since as.epidata.default() checked that events (I) can only
### happen when at risk (S). This check can now be disabled (with a warning).
### Furthermore, twinSIR() requires amendments for such data.
################################################################################

devtools::load_all("~/Projekte/surveillance/pkg")
##library("surveillance")

## minimal example using the hagelloch data
data("hagelloch")
hagelloch.df <- hagelloch.df[order(hagelloch.df$tI),]
head(hagelloch.df)
seirdata <- hagelloch.df[1:4, c("PN", "tI", "tR", "x.loc", "y.loc")]
set.seed(1)
seirdata$tE <- seirdata$tI - rnorm(nrow(seirdata), mean = 9, sd = 2)
seirdata

## ignore latent period and setup conventional SIR data
sirepi <- as.epidata(seirdata, t0 = 0, tI.col = "tI", tR.col = "tR",
                     id.col = "PN", coords.cols = c("x.loc", "y.loc"))
summary(sirepi)
plot(sirepi)

## with latent period
##debugonce(as.epidata.data.frame)
seirepi <- as.epidata(seirdata, t0 = 0, tE.col = "tE", tI.col = "tI", tR.col = "tR",
                      id.col = "PN", coords.cols = c("x.loc", "y.loc"))
##Error in as.epidata.default(data = evHist, id.col = "id", start.col = "start",  :
##  inconsistent atRiskY/event indicators in row 6: event only if at risk

summary(seirepi)  ## BUG: only 184 is initially infectious
## FIXME: need to be able to overwrite default atRiskY==0 interpretation for
##        initially infectious in as.epidata.default, update.epidata, [.epidata
##        (and potentially other functions)

summary(seirepi)$counters  ## BUG: nS and nI are wrong
plot(seirepi)  ## uses "counters" so is also wrong


## try fitting a simple twinSIR without the epidemic component

summary(twinSIR(~1, data = sirepi))  # ok

twinSIR(~1, data = seirepi)
## this is probably wrong


### check with whole data and pseudo E events

hagellochFit <- twinSIR(~household + c1 + c2 + nothousehold, data = hagelloch)

hagelloch.df$tE <- hagelloch.df$tI - 0.5
hagellochE <- as.epidata(
    hagelloch.df, t0 = 0, tE.col = "tE", tI.col = "tI", tR.col = "tR",
    id.col = "PN", coords.cols = c("x.loc", "y.loc"),
    f = list(
        household    = function(u) u == 0,
        nothousehold = function(u) u > 0
    ),
    w = list(
        c1 = function (CL.i, CL.j) CL.i == "1st class" & CL.j == CL.i,
        c2 = function (CL.i, CL.j) CL.i == "2nd class" & CL.j == CL.i
    ),
    keep.cols = c("SEX", "AGE", "CL"))

stopifnot(all.equal(summary(hagellochE), summary(hagelloch)))

hagellochFitE <- twinSIR(~household + c1 + c2 + nothousehold, data = hagellochE)

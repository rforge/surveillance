
### twinSIR() bug in surveillance 1.15.0:
### 'subset' is ignored for 'nEvents' iff nIntervals = 1,
### and in attr(model$survs, "eventTimes"), which also affects
### automatic 'knots', residuals() and the rug in intensityplot()

library("surveillance")
## devtools::load_all("~/Projekte/surveillance/pkg/")

data("hagelloch")

## check nEvents
assert_nEvents <- function (fit)
{
    nEvents <- sum(fit$model$survs$event)
    stopifnot(
        sum(fit$nEvents) == nEvents,
        length(attr(fit$model$survs, "eventTimes")) == nEvents,
        length(residuals(fit)) == nEvents
    )
}

## standard fit
hagellochFit <- twinSIR(~household, data = hagelloch)
summary(hagellochFit)
assert_nEvents(hagellochFit)
## ok

## fit a subset of the events using the default nIntervals = 1
hagellochFitSubset1 <- twinSIR(~household, data = hagelloch,
                               subset = CL == "preschool", nIntervals = 1)
summary(hagellochFitSubset1)  # total number of infections is wrong!
assert_nEvents(hagellochFitSubset1)
## fails in surveillance <= 1.15.0

## fit a subset of the events using nIntervals > 1
hagellochFitSubset2 <- twinSIR(~household, data = hagelloch,
                               subset = CL == "preschool", nIntervals = 2)
summary(hagellochFitSubset2)
## number of events is correct
## but eventTimes are taken from the whole data!
assert_nEvents(hagellochFitSubset2)
## fails in surveillance <= 1.15.0

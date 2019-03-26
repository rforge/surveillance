#!/usr/bin/env Rscript
### fit the siaf.step model from the JSS paper and do profiling

##devtools::load_all("~/Projekte/surveillance/pkg")
library("surveillance")

## tie-breaking
data("imdepi", package = "surveillance")
eventDists <- dist(coordinates(imdepi$events))
(minsep <- min(eventDists[eventDists > 0]))
set.seed(321)
imdepi_untied <- untie(imdepi, amount = list(s = minsep / 2))
imdepi_untied_infeps <- update(imdepi_untied, eps.s = Inf)

## model formulae
endemic <- addSeason2formula(~offset(log(popdensity)) + I(start / 365 - 3.5),
                             period = 365, timevar = "start")
epidemic <- ~type + agegrp

## fit endemic-only model
imdfit_endemic <- twinstim(
    endemic = endemic,
    data = imdepi_untied_infeps, subset = !is.na(agegrp),
    model = TRUE)

## fit full model (using parameter estimates from above)
#library("profvis")
#prof <- profvis(
imdfit_step4 <- update(imdfit_endemic, epidemic = epidemic,
  siaf = siaf.step(exp(1:4 * log(100) / 5), maxRange = 100),
  start = c("e.(Intercept)" = -10, setNames(-2:-5, paste0("e.siaf.", 1:4))))
#)
#print(prof)

## load benchmark results
if (FALSE) {
    benchmark <- imdfit_step4
    save(benchmark, file = "benchmark_step.RData")
} else {
    ## produced 11 May 2018 with R-3.4.4, surveillance 1.16.0
    load("benchmark_step.RData")
}

all.equal(benchmark, imdfit_step4)

benchmark$runtime
imdfit_step4$runtime

## runtime of single likelihood and score evaluations (expected fisher has no integral)
runtimes <- t(vapply(c("ll", "sc"), function (which)
    print(system.time(imdfit_step4$functions[[which]](
        jitter(coef(imdfit_step4), amount = 1e-12)))),
    FUN.VALUE = numeric(5)))
runtimes[,"elapsed"]

## surveillance 1.16.1:
## R-3.4.4: 10.9 s, 0.57 s, 1.17 s
## R-3.5.3: 6.9 s, 0.19 s, 0.37 s

## surveillance 1.17.0:
## R-3.5.3: 5.6 s, 0.17 s, 0.33 s

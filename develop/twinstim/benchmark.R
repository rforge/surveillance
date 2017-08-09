#!/usr/bin/env Rscript
### fit the power-law model from the JSS paper and measure runtime

#devtools::load_all("~/Projekte/polyCub")
devtools::load_all("~/Projekte/surveillance/pkg")

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
imdfit_powerlaw <- update(imdfit_endemic,
    epidemic = epidemic, siaf = siaf.powerlaw(engine = "C"),
    start = c("e.(Intercept)" = -6, "e.siaf.1" = 1.5, "e.siaf.2" = 1),
    cores = 2)
#)
#print(prof)

## load benchmark results
if (FALSE) {
    benchmark <- imdfit_powerlaw
    save(benchmark, file = "benchmark.RData")
} else {
    ## produced 16 May 2017 with R-3.3.3, surveillance 1.13.1, polyCub 0.5-2
    load("benchmark.RData")
}

all.equal(benchmark, imdfit_powerlaw)

imdfit_powerlaw$runtime

## runtime of single likelihood and score evaluations (expected fisher has no integral)
runtimes <- t(vapply(c("ll", "sc"), function (which)
    print(system.time(imdfit_powerlaw$functions[[which]](
        jitter(coef(imdfit_powerlaw), amount = 1e-12)))),
    FUN.VALUE = numeric(5)))
runtimes[,"elapsed"]

## benchmarks on 16 May 2017 using surveillance 1.13.1:
## R-3.3.3, polyCub 0.5-2 (benchmark):            242 s
## R-3.4.0, polyCub 0.5-2:                        261 s
## R-3.3.3, polyCub 0.5-2 w/ registered routines: 242 s
## R-3.4.0, polyCub 0.5-2 w/ registered routines: 267 s

## benchmarks on 2 June 2017 using surveillance 1.13.1 and polyCub 0.6.0:
## R-3.3.3: 235 s, 7.5 s, 24.0 s
## R-3.4.0: 240 s, 8.0 s, 25.6 s

## surveillance 1.14.0, polyCub 0.6.0:
## R-3.3.3, engine=R: 235 s, 7.8 s, 25.0 s
## R-3.3.3, engine=C:  49 s, 1.6 s,  5.2 s
## R-3.4.1, engine=R: 252 s, 8.3 s, 26.9 s
## R-3.4.1, engine=C:  47 s, 1.4 s,  4.9 s

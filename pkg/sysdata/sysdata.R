#!/usr/bin/env Rscript
### Rscript file for packing internal datasets


### Load data for internal use

## confidence intervals for the mean of a poisson variable
## used in the algo.rki function. This table is from
## Sachs, "Angewandte Statistik". Results are similar, but
## not identical to the results of a call
## t(sapply(0:20, function(x) c(x,poisson.test(x)$conf.int)))
## Note: Sample size is not taken into account!!
CIdata <- read.table("CIdata.txt", header=TRUE)

## pre-calculated values of the hypergeometric 2F1 function
## for parameters c(1/3,2/3) and 5/3
## (needed for computing Anscombe residuals in algo.cusum() in case Y ~ NegBin)
## atm this is computed for the values seq(0,10,by=0.01) and 11:100
surveillance.gvar.hyp <- scan("hypGeomSmall.txt")
surveillance.gvar.z   <- - c(0:1000/100, 11:100)

## extended list of references (methodological papers)
REFERENCES <- readCitationFile(
    file = "REFERENCES",
    meta = packageDescription("pkg", lib.loc = "../..")
)

## a simple polygonal "owin" (with hole) for testing, obtained as
## spatstat::owin(poly = rapply(
##     spatstat::shift.owin(spatstat.data::letterR, -c(3,2))$bdry,
##     round, digits = 1, how = "replace"))
LETTERR <- spatstat::owin(poly = list(
    list(x = c(0.9, 0.8, 0.7, 0.5, 0.4, 0.5, 0.7, 0.8, 0.8, 0.7, 0.7, 0.5,
               0.3, -1, -1, -0.3, -0.3, -0.1, 0, 0.3, 0.9),
         y = c(-1.3, -1, -0.7, -0.3, -0.2, -0.1, 0.1, 0.3, 0.5, 0.8, 1,
               1.2, 1.3, 1.3, -1.3, -1.3, -0.3, -0.3, -0.5, -1.3, -1.4)),
    list(x = c(-0.4, -0.4, 0, 0.1, 0.2, 0.1, 0.1, 0),
         y = c(0.2, 0.7, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2))
    ))


### Save to sysdata.rda

save(list=ls(), file="sysdata.rda")

## try to improve compression
tools::resaveRdaFiles("sysdata.rda")

## The resulting sysdata.rda file has to be moved to "R/sysdata.rda",
## which is automatically done in the Makefile

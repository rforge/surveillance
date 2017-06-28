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


### Save to sysdata.rda

save(list=ls(), file="sysdata.rda")

## try to improve compression
tools::resaveRdaFiles("sysdata.rda")

## The resulting sysdata.rda file has to be moved to "R/sysdata.rda",
## which is automatically done in the Makefile

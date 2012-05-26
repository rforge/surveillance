#! /usr/local/bin/Rscript

# Rscript file for packing internal datasets

### Load data for internal use

#confidence intervals for the mean of a poisson variable
#used in the algo.rki function. This table is from
#Sachs, "Angewandte Statistik". Results are similar, but
#not identical to the results of a call 
#t(sapply(0:20, function(x) c(x,poisson.test(x)$conf.int)))
#Note: Sample size is not taken into account!!
CIdata <- read.table("CIdata.txt", header=TRUE)

#precalculated values of the hyper geometric 2F1 function
surveillance.gvar.hyp <- scan("hypGeomSmall.txt")
surveillance.gvar.z   <- - c(0:1000/100, 11:100)

### Save to sysdata.rda

save(list=ls(), file="sysdata.rda")

# try to improve compression
tools::resaveRdaFiles("sysdata.rda")

#The file has to be copied to "R/sysdata.rda"

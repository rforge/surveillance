#! /usr/local/bin/Rscript

# Rscript file for packing internal datasets


### Load data for internal use

CIdata <- read.table("CIdata.txt", header=TRUE)


### Save to sysdata.rda

save(list=ls(), file="sysdata.rda")

# try to improve compression
tools::resaveRdaFiles("sysdata.rda")


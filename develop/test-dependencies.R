################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[test-dependencies.R] by SM Die 24/06/2014 22:21 (CEST)>
###
### Find out which packages from "Suggests" are required to run the tests
### -> Inform Debian package r-cran-surveillance 
################################################################################

library("surveillance")
library("testthat")

## now reset library paths to only contain R's default library in R_HOME
orig.libPaths <- .libPaths()
.libPaths(.Library)

## run the package tests and watch failures due to missing packages
setwd("~/Projekte/surveillance/pkg/tests")
test_check("surveillance")

## => required packages: maxLik

loadNamespace("maxLik", lib.loc=orig.libPaths)

test_check("surveillance")
## now ok

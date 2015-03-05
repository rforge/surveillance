library("INLA")
library("testthat")

library("delayPackage")
library("surveillance")
##################################################################
context("Checking the provided reporting triangle")
# Control slot for the proposed algorithm with D=10 correction
rangeTest <- 410:412
alpha <- 0.05

controlDelay <-  list(range = rangeTest, b = 4, w = 3,
                      pastAberrations = TRUE, mc.munu=10, mc.y=10,
                      verbose = FALSE,populationOffset=FALSE,
                      alpha = alpha, trend = TRUE,
                      limit54=c(0,50),
                      noPeriods = 10, pastWeeksNotIncluded = 26,
                      delay=TRUE)
test_that("The absence of reporting triangle throws an error",{
  data("salmNewport")
  expect_error(algoDelayNorm(salmNewport, controlDelay),"You have to")
})
test_that("The function spots uncorrect reporting triangles",{
  data('salmAllOnset')
  stsFake <- salmAllOnset
  stsFake@control$reportingTriangle$n <- head(stsFake@control$reportingTriangle$n,n=10)
  expect_error(algoDelayNorm(stsFake, controlDelay),"The reporting triangle number")
  stsFake <- salmAllOnset
  stsFake@control$reportingTriangle$n[1,] <- stsFake@control$reportingTriangle$n[1,]/2
  expect_error(algoDelayNorm(stsFake, controlDelay),"The reporting triangle is wrong")
})

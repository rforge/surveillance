loadLib <- function() {
  #Move to development dir
  setwd("~/Surveillance/surveillance/develop/spatscan/")

  cases <- scan("data/breast.cas",quiet=TRUE)
  population <- scan("data/breast.pop",quiet=TRUE)
  geo <- matrix(scan("data/breast.geo",quiet=TRUE),ncol=2,byrow=TRUE)
  n <- nrow(geo)
  mc.iter <- 100 # number of Monte Carlo iterations
  if ((length(cases) != n) | (length(population) != n)) {
    stop("Error: cases, population and geo are not of the same length")
  }
  
  #Load the C code (this will be moved into the package soon)
  dyn.load("spatscan.so")


  return(is.loaded("WrapScanClusterBernoulli"))
}

test <- function() {
  source("test-spatscan.R")
  loadLib()
  res <- .C("WrapScanClusterBernoulli", size=as.integer(0), indices=as.integer(rep(0,n)), prob=as.double(0),pval=as.double(0), centroids=as.integer(n),population=as.double(population),cases=as.double(cases),geox=as.double(geo[,1]),geoy=as.double(geo[,2]),mc.iter=as.integer(mc.iter),tx=as.double(0),ret_val=as.integer(1))
  res$size
  res$pval
  res$prob
  res$indices
  
  dyn.unload("spatscan.so")
}

foo <- function() {
}

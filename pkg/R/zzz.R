### R code from vignette source 'Rnw/zzz.Rnw'

###################################################
### code chunk number 1: zzz.Rnw:1-21
###################################################

.onLoad <- function(libname, pkgname) {
  #Load the CIdata thing
  data(CIdata, package=pkgname)

  #Read the table of the hypgeom_2F1 function for parameters c(1/3,2/3) and
  #5/3 -- atm this is computed for the values seq(0,10,by=0.01) and 11:100
  #Load the pre-evaluated Hypergeometric function for computing Anscombe residuals
  data("hypGeomSmall",package=pkgname)
  
  #License limitation for package gpclib. Now as part of 
  #packageStartupMessage
  packageStartupMessage(paste("\n\tNote: polygon geometry computations related to",
      "\tthe \"epidataCS\" class in surveillance depend on",
      "\tthe package gpclib, which has a restricted licence.\n",
      sep="\n"),appendLF=FALSE)

}





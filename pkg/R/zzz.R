### R code from vignette source 'Rnw/zzz.Rnw'

###################################################
### code chunk number 1: zzz.Rnw:1-35
###################################################

#.First.lib <- function(libname, pkgname) { #this works for R < 2.14
.onLoad <- function(libname, pkgname) {
  #Load the necessary packages
  
# Removed as now part of namespace  
#  library(spc)
#  library(maptools)
#  library(msm)

  #Load the CIdata thing
  data(CIdata, package=pkgname)

  #Read the table of the hypgeom_2F1 function for parameters c(1/3,2/3) and
  #5/3 -- atm this is computed for the values seq(0,10,by=0.01) and 11:100
  #Load the pre-evaluated Hypergeometric function for computing Anscombe residuals
  data("hypGeomSmall",package=pkgname)
  
 # surveillance.gvar.hyp <<- scan(file.path(.path.package('surveillance'),'data',"hypGeomSmall.txt"),quiet=TRUE)
 # surveillance.gvar.z <<- - c(0:1000/100, 11:100)

# Removed. Now part of namespace
#  #Load the C code library for the glr stuff  
# library.dynam("surveillance", pkgname, libname)
  
  #License limitation for package gpclib
  cat("\n\tNote: polygon geometry computations related to",
      "\tthe \"epidataCS\" class in surveillance depend on",
      "\tthe package gpclib, which has a restricted licence.\n",
      sep="\n")

}





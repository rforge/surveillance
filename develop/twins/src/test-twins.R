setwd("~/Surveillance/surveillance/develop/twins/src")
library("surveillance")

#Read data as disProg object
#x <- scan("../data/hepatitisA.txt",quiet=TRUE)[-1]
#x <- create.disProg(week=1:length(x),observed=x,state=numeric(length(x)),start=c(2001,1), freq=52)
#hepatitisA <- x
#save(file="~/Surveillance/surveillance/pkg/data/hepatitisA.RData",list=c("hepatitisA"))

#Load the C code (this will be moved into the package soon)
dyn.load("twins.so")
is.loaded("twins")



#Load algorithm
source("algo_twins.R")

#New versions of the package contain the data
data("hepatitisA")

#Fix seed - this is used for the MCMC in twins
set.seed(123)

obj <- algo.twins(hepatitisA)

#This shows the entire output
plot(obj)


setwd("~/Surveillance/surveillance/develop/twins/src")
library("surveillance")

#Read data as disProg object
x <- scan("../data/hepatitisA.txt",quiet=TRUE)[-1]
x <- create.disProg(week=1:length(x),observed=x,state=numeric(length(x)),start=c(2001,1), freq=52)

#Load the C code 
dyn.load("twins.so")
is.loaded("twins")

#Fix seed
set.seed(123)


source("algo_twins.R")
#debug(algo.twins)
obj <- algo.twins(x)

plot(obj)


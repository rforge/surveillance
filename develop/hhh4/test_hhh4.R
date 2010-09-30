library(surveillance)
source("R/algo_hhh4.R")

load("data/flu-BYBW.RData") 
# use reduced data set for testing 
sts.flu <- sts.flu[1:200,1:80]

###############################################################
## generate formula for temporal and seasonal trends
# in fact, this should be done automatically in algo.hhh4...
f.end <- addSeason2formula(f = ~ -1 + ri(type="iid"), S=1, period=52)

cntrl <- list(ar = list(f = ~ 1),
              end = list(f =f.end, offset = population(sts.flu)),
              verbose=1, data=data.frame(t=epoch(sts.flu)-1))
               
res <- hhh4(sts.flu,cntrl)



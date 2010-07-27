library(surveillance)
source("R/algo_hhh4.R")

load("data/flu-BYBW.RData") 
# use reduced data set for testing 
sts.flu <- sts.flu[1:200,1:80]

###############################################################
## generate "covariates" for temporal and seasonal trends
# in fact, this should be done automatically in algo.hhh4...

week <- epoch(sts.flu)-1

form<-function(mod="~-1",S=1, period=sts.flu@freq){
  if(S>0){
    for(i in 1:S){
      mod <- paste(mod,"+sin(",2*i,"*pi*t/",period,")+cos(",2*i,"*pi*t/",period,")",sep="")
    }
  }
  return(as.formula(mod))
}

X.Season<-model.matrix(form(),data.frame(t=week))
sin1 <- X.Season[,1]
cos1 <- X.Season[,2]

cntrl <- list(ar = list(f = ~ 1),
              end = list(f = ~ -1+ri(type="iid") +fe(sin1) + fe(cos1), offset = population(sts.flu)),
              verbose=1)
               
res <- algo.hhh4(sts.flu,cntrl)




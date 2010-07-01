require(surveillance)

# source functions by Sereina Herzog
source("R/algo_hhh_covariates.R")

# adjustment: vacc coverage among students without vacc card is 
# 0.5 times as high as among students with vacc. cards
adjustVac <- function(cardVac, p=0.5,nrow=1){
  card <- cardVac[,1]
  vac <- cardVac[,2]
  vacAdj <- vac*card + p*vac*(1-card)
  return(matrix(vacAdj,nrow=nrow, ncol=length(vacAdj), byrow=TRUE))
}

# load measles and vaccination coverage data from the RKI
# see Herzog et al (2010), Epidemiology and Infection for info
load("data/measles_GER-BL_2005-2007.RData")

# use bi-weekly aggregated measles data
nfreq <- 26
nYears <- 3

# use log proportion of unvaccinated school starters, adjusted with factor 0.5
vac0 <- log(1-adjustVac(vac2006[1:16,3:4],p=0.5,nrow=nfreq*nYears))

# model with overall intercept, S=1 season, autoregressive parameter
cntrl <- list(nseason=1,
              interceptnu="one",
              offset=TRUE,
              link="log",
              lambda=TRUE,
              names=TRUE)

A0 <- algo.hhhcov(measles2w, control=cntrl, covlambda=list(vac0=vac0))
B0 <- algo.hhhcov(measles2w, control=cntrl, covnu=list(vac0=vac0))
C <- algo.hhhcov(measles2w, control=cntrl)

cntrl2 <- list(nseason=1, interceptnu="one",offset=TRUE,link="log",lambda=FALSE, names=TRUE)
D <- algo.hhhcov(measles2w, control=cntrl2)



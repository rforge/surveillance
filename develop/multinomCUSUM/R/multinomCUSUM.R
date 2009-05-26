#########################################################################
# Multinomial CUSUM for y_t \sim M_k(n_t, \pi_t) for t=1,...,tmax
# Workhorse function doing the actual computations - no semantic checks
# are performed here, we expect "proper" input.
#
# Params:
#  y - (k-1) \times tmax observation matrix for all but the reference category
#  pi0 - (k-1) \times tmax in-control prob vector for all but ref cat
#  pi1 - (k-1) \times tmax out-of-control prob vector for all but ref cat
#  n   - vector of dim tmax containing the varying sizes
#  h   - decision threshold of the multinomial CUSUM
#########################################################################

multinomialCUSUM <- function(y, pi0, pi1, n, h) {
  #Initialize variables
  t <- 0
  stopped <- FALSE
  S <- numeric(ncol(y)+1)
  
  #Run the Multinomial LR CUSUM
  while (!stopped) {
    #Increase time
    t <- t+1
    #Compute log likelihood ratio
    llr <- dmultinom(y[,t], size=n[t], prob=pi1[,t],log=TRUE) -
      dmultinom(y[,t], size=n[t], prob=pi0[,t],log=TRUE)
    #Add to CUSUM
    S[t+1] <- max(0,S[t] + llr)

    #Only run to the first alarm. Then reset.
    if ((S[t+1] >= h) | (t==ncol(y))) { stopped <- TRUE}
  }
  #If no alarm at the end put rl to end (its censored!)
  if (sum(S[-1]>h)>0) {
    t <- which.max(S[-1] > h)
  } else {
    t <- ncol(pi0) ##Last one
  }
  #Missing: cases needs to be returned!
  return(list(N=t,val=S[-1],cases=NULL))
}

######################################################################
# Wrap function to process disProg object by multinomCUSUM (new S4
# style). Time varying number of counts is found in slot populationFrac.
#
# Params:
#  control - list with the following components
#    * range - vector of indices in disProgObj to monitor
#    * h     - threshold, once CUSUM > h we have an alarm
#    * pi0 - (k-1) \times tmax in-control prob vector for all but ref cat
#    * pi1 - (k-1) \times tmax out-of-control prob vector for all but ref cat
######################################################################

multinomCUSUM <- function(stsObj,
                               control = list(range=NULL,h=5,
                                 pi0=NULL, pi1=NULL, ret=c("cases","value"))) {

  # Set the default values if not yet set
  if(is.null(control[["pi0",exact=TRUE]])) { 
    stop("Error: No specification of in-control proportion vector pi0!")
  }
  if(is.null(control[["pi1",exact=TRUE]])) { 
    stop("Error: No specification of out-of-control proportion vector pi1!")
  }
  if(is.null(control[["h",exact=TRUE]]))
    control$h <- 5
  if(is.null(control[["ret",exact=TRUE]]))
  	control$ret <- "value"

  #Extract the important parts from the arguments
  range <- control$range
  y <- t(stsObj@observed[range,,drop=FALSE])
  pi0 <- control[["pi0",exact=TRUE]]
  pi1 <- control[["pi1",exact=TRUE]]
  control$ret <- match.arg(control$ret, c("value","cases"))
  ret <- pmatch(control$ret,c("value","cases"))
  ##n contains number. Same for all. Alternative: sum over y's
#  n <- as.numeric(stsObj@populationFrac[range,1,drop=FALSE])  #just take first
  n <- apply(y, 2, sum)
  
  #Semantic checks
  if ( ((ncol(y) != ncol(pi0)) | (ncol(pi0) != ncol(pi1))) |
      ((nrow(y) != nrow(pi0)) | (nrow(pi0) != nrow(pi1)))) {
    stop("Error: dimensions of y, pi0 and pi1 have to match")
  }
  if (length(n) != ncol(y)) {
    stop("Error: Length of n has to be equal to number of columns in y")
  }
  #Check if all n entries are the same
  if (!all(apply(stsObj@populationFrac[range,],1,function(x) all.equal(as.numeric(x),rev(as.numeric(x)))))) {
    stop("Error: All entries for n have to be the same in populationFrac")
  }
  
  #Reserve space for the results
  # start with cusum[timePoint -1] = 0, i.e. set cusum[1] = 0
  alarm <- matrix(data = 0, nrow = length(range), ncol = nrow(y))
  upperbound <- matrix(data = 0, nrow = length(range), ncol = nrow(y))

  #Small helper function to be used along the way --> move to other file!
  either <- function(cond, whenTrue, whenFalse) {
    if (cond) return(whenTrue) else return(whenFalse)
  }

  
  #Setup counters for the progress
  doneidx <- 0
  N <- 1
  noofalarms <- 0
  noOfTimePoints <- length(range)

  #######################################################
  #Loop as long as we are not through the entire sequence
  #######################################################
  while (doneidx < noOfTimePoints) {
     ##Run multinomial CUSUM until the next alarm
    res <- multinomialCUSUM(y, pi0, pi1, n, h=control$h)

    #In case an alarm found log this and reset the chart at res$N+1
    if (res$N < ncol(y)) {
      #Put appropriate value in upperbound
      upperbound[1:res$N + doneidx,]  <- matrix(rep(either(ret == 1, res$val[1:res$N] ,res$cases[1:res$N]),each=ncol(upperbound)),ncol=ncol(upperbound),byrow=TRUE)
      alarm[res$N + doneidx,] <- TRUE

      #Chop & get ready for next round
      y <- y[,-(1:res$N),drop=FALSE]
      pi0 <- pi0[,-(1:res$N),drop=FALSE]
      pi1 <- pi1[,-(1:res$N),drop=FALSE]
      n <- n[-(1:res$N)]
      
      #Add to the number of alarms
      noofalarms <- noofalarms + 1
    }
    doneidx <- doneidx + res$N
  }

  #Add upperbound-statistic of last segment, where no alarm is reached
  upperbound[(doneidx-res$N+1):nrow(upperbound),]  <- matrix( rep(either(ret == 1, res$val, res$cases),each=ncol(upperbound)),ncol=ncol(upperbound),byrow=TRUE)
  

  
  # Add name and data name to control object
  control$name <- "multinomCUSUM"
  control$data <- NULL #not supported anymore


  #New direct calculations on the sts object
  stsObj@observed <- stsObj@observed[control$range,,drop=FALSE]
  stsObj@state <- stsObj@state[control$range,,drop=FALSE]
  stsObj@populationFrac <- stsObj@populationFrac[control$range,,drop=FALSE]
  stsObj@alarm <- alarm
  stsObj@upperbound <- upperbound

  #Fix the corresponding start entry
  start <- stsObj@start
  new.sampleNo <- start[2] + min(control$range) - 1
  start.year <- start[1] + (new.sampleNo - 1) %/% stsObj@freq 
  start.sampleNo <- (new.sampleNo - 1) %% stsObj@freq + 1
  stsObj@start <- c(start.year,start.sampleNo)

  #Ensure dimnames in the new object ## THIS NEEDS TO BE FIXED!
  #stsObj <- fix.dimnames(stsObj)

  #Done
  return(stsObj)
}

######################################################################
# Tester function
######################################################################

testIt <- function() {
  source("multinomCUSUM.R")
  load("../data/pediatrist.RData")
  library("surveillance")  
  library("nnet")

  source("~/Surveillance/surveillance/pkg/R/sts.R")
  ##Problem with plot function?!?!?!
debug("plot.sts.time.one")
#  pediatrist@state <- pediatrist@observed*0
#  pediatrist@alarm <- pediatrist@observed*0
#  pediatrist@upperbound <- pediatrist@observed*0
  plot(pediatrist)

  #Training and test data
  train <- 1:24 
  range <- 25:42

  #Fit multinomial model use 1-2 as reference category. For fitting remove rows
  #among training data with rowsum zero
  rowsumZero <- apply(pediatrist@observed[train,],1,sum) == 0
  dfTrain <- data.frame(pediatrist@observed[train,][!rowsumZero,,drop=FALSE],t=train[!rowsumZero])
  
  m.pA <- multinom( I(as.matrix(dfTrain[,c(2,1,3,4,5)])) ~ 1 + t  + sin(2*pi/12*t) + cos(2*pi/12*t), data=dfTrain)

  #Probability vectors under 0 and 1
  theta0 <- coef(m.pA)
  R <- c(1,1,1,1)
  theta1 <- cbind(theta0[,1]+R,theta0[,-1])

  #Time
  period <- 12
  t <- range
  
  #Create covariate matrix
  x <- matrix(cbind(1,t,sin(2*pi*t/period),cos(2*pi*t/period)),nrow=length(t))

  #Calculate probability of each class under H0 and H1
  pi0 <- apply(exp(theta0 %*% t(x)), 2, function(x) x/(1+sum(x)))
  pi1 <- apply(exp(theta1 %*% t(x)), 2, function(x) x/(1+sum(x)))

  #Compute probabilities and y for all categories (i.e. not leaving out the last)
  pi1 <- rbind(pi1,1-apply(pi1,2,sum))
  pi0 <- rbind(pi0,1-apply(pi0,2,sum))

  source("multinomCUSUM.R")
#  debug("multinomCUSUM")
  surv <- multinomCUSUM(pediatrist, control=list(range=range, pi=pi0, pi0=pi0, pi1=pi1, h=4))

  #Problem: no real interpretation of CUSUM statistic
  surv@upperbound <- surv@upperbound / 5
  
  hookFunc <- function() {
    matlines(1:ncol(pi0),cbind(pi0[k,],pi1[k,]),col=c("green","red"),lwd=2)
    axis(4,at=seq(0,max(surv@upperbound),by=1/5),labels=seq(0,max(surv@upperbound)*5,by=1),line=-0.5,mgp=c(3,-2,0),col="blue")
  }
  #Plot all but reference category
  plot(surv[,1:4],par.list=list(mar=c(4,1,1,1)),dx.upperbound=0, hookFunc=hookFunc)
   


}

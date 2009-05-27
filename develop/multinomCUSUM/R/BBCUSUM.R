#########################################################################
# Beta binomial CUSUM for y_t \sim BB(n_t, \pi_t, sigma) for t=1,...,tmax
# Workhorse function doing the actual computations - no semantic checks
# are performed here, we expect "proper" input.
#
# Params:
#  y - (k) \times tmax observation matrix for all categories
#  pi0 - (k) \times tmax in-control prob vector for all categories
#  pi1 - (k) \times tmax out-of-control prob vector for all categories
#  n   - vector of dim tmax containing the varying sizes
#  h   - decision threshold of the multinomial CUSUM
#########################################################################

BBCUSUM.calc <- function(y, pi0, pi1, n, h, sigma) {
  #Initialize variables
  t <- 0
  stopped <- FALSE
  S <- numeric(ncol(y)+1)
  U <- numeric(ncol(y)+1)
  
  #Run the Multinomial LR CUSUM
  while (!stopped) {
    #Increase time
    t <- t+1
    #Compute log likelihood ratio
    llr <- dBB(y[,t], bd=n[t], mu=pi1[,t],sigma=sigma,log=TRUE) -
      dBB(y[,t], bd=n[t], mu=pi0[,t],sigma=sigma,log=TRUE)
    #Add to CUSUM
    S[t+1] <- max(0,S[t] + llr)

    #For binomial data it is also possible to compute how many cases it would take
    #to sound an alarm given the past.
    if (nrow(y) == 2) {
      #Calculations in ../maple/numberneededbeforealarm.mw
      at <- 0# (h - S[t] - n[t] * ( log(1 - pi1[1,t]) - log(1-pi0[1,t]))) / (log(pi1[1,t]) - log(pi0[1,t]) - log(1-pi1[1,t]) + log(1-pi0[1,t]))
      U[t+1] = ceiling(max(0,at))
    }
    
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
  return(list(N=t,val=S[-1],cases=U[-1]))
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

betabinomialCUSUM <- function(stsObj,
                               control = list(range=NULL,h=5,
                                 pi0=NULL, pi1=NULL, sigma=NULL,ret=c("cases","value"))) {

  # Set the default values if not yet set
  if(is.null(control[["pi0",exact=TRUE]])) { 
    stop("Error: No specification of in-control proportion vector pi0!")
  }
  if(is.null(control[["pi1",exact=TRUE]])) { 
    stop("Error: No specification of out-of-control proportion vector pi1!")
  }
  if(is.null(control[["h",exact=TRUE]]))
    control$h <- 5
    if(is.null(control[["sigma",exact=TRUE]]))
    control$h <- 1
  if(is.null(control[["ret",exact=TRUE]]))
  	control$ret <- "value"

  #Extract the important parts from the arguments
  range <- control$range
  y <- t(stsObj@observed[range,,drop=FALSE])
  pi0 <- control[["pi0",exact=TRUE]]
  pi1 <- control[["pi1",exact=TRUE]]
  control$ret <- match.arg(control$ret, c("value","cases"))
  ret <- pmatch(control$ret,c("value","cases"))
  n <- as.numeric(stsObj@populationFrac[range,1,drop=FALSE])  
  
  #Semantic checks
  if ( ((ncol(y) != ncol(pi0)) | (ncol(pi0) != ncol(pi1))) |
      ((nrow(y) != nrow(pi0)) | (nrow(pi0) != nrow(pi1)))) {
    stop("Error: dimensions of y, pi0 and pi1 have to match")
  }
  if ((control$ret == 2) & ncol(pi0) != 2) {
    stop("Cases can only be returned in case of binomial, i.e. k=2")
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
    res <- BBCUSUM.calc(y, pi0, pi1, n, h=control$h, sigma=control$sigma)

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
  control$name <- "BBCUSUM"
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

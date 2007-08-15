###################################################
### chunk number 1: 
###################################################

algo.hmm <- function(disProgObj, control = list(range=range, noStates=2, trend=TRUE, noHarmonics=1,covEffectEqual=FALSE )){

  # Set the default values if not yet set
  if(is.null(control$noStates)){ control$noStates <- 2 }
  if(is.null(control$trend)){ control$trend <- TRUE }
  if(is.null(control$noHarmonics)){ control$noHarmonics <- 1 }
  if(is.null(control$covEffectEqual)){ control$covEffectEqual <- FALSE }

  #Initialize some often used vectors
  t <- control$range
  observed <- disProgObj$observed[t]


  #Init data
  counts <- data.frame(observed, t)
  names(counts) <- c("observed","t")
  formulaStr <- ifelse(control$trend, "~ 1 + t ", "~ 1 ")
  #Create formula and add harmonics as covariates
  for (i in seq_len(control$noHarmonics)) {
    counts[,paste("cos",i,"t",sep="")] <- cos(2*i*pi*(t-1)/disProgObj$freq)
    counts[,paste("sin",i,"t",sep="")] <- sin(2*i*pi*(t-1)/disProgObj$freq)
    formulaStr <- paste(formulaStr,"+ cos",i,"t + sin",i,"t ",sep="")
  }
  
  #Obtain crude inits
  q <- quantile(observed,seq(0,1,length=control$noStates+1))
  lvl <- cut(observed,breaks=q,include.lowest=TRUE)
  crudeMean <- as.numeric(tapply(observed, lvl, mean))

  hcovariates <- list()
  hmodel <- list()
  for (i in seq_len(control$noStates)) {
    hcovariates[[i]] <- as.formula(formulaStr)
    val <- crudeMean[i]
    #Substitution necessary, as hmmPois does lazy evaluation of rate argument
    hmodel[[i]] <- eval(substitute(hmmPois(rate=val),list(val=crudeMean[i])))
  }

  #Any constraints on the parameters of the covariates for the different states
  hconstraint <- list()
  if (control$covEffectEqual) {
    hconstraint <- list(t=rep(1,control$noStates))
    for (i in seq_len(control$noHarmonics)) {
      hconstraint[[paste("sin",i,"t",sep="")]] <- rep(1,control$noStates)
      hconstraint[[paste("cos",i,"t",sep="")]] <- rep(1,control$noStates)
    }
  }


  # fit the HMM
  hmm <- msm(observed ~ t, data=counts,
             #Two state HMM with initial values
             qmatrix = matrix(1/control$noStates,control$noStates,control$noStates),
             #y|x \sim Po( \mu[t] ) with some initial values
             hmodel = hmodel,
             #Models for \log \mu_t^1 and \log \mu_t^2
             hcovariates = hcovariates,
             #Force the effects of the trend and harmonics to be equal for all states
             hconstraint=hconstraint
             )
  #If most probable state equals the high count state then do alarm 
  alarm <- matrix(viterbi.msm(hmm)$fitted,ncol=1) == control$noStates
  #Upperbound does not have any meaning
  upperbound <- alarm * 0
  
  #Add name and data name to control object.
  control$name <- paste("hmm:", control$trans)
  control$data <- paste(deparse(substitute(disProgObj)))
  control$hmm  <- hmm

  # return alarm and upperbound vectors
  result <- list(alarm = alarm, upperbound = upperbound, disProgObj=disProgObj,control=control)

  class(result) = "survRes" # for surveillance system result
  return(result)
}



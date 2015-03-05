#     ____________________________
#    |\_________________________/|\
#    ||                         || \
#    ||    algoDelay            ||  \
#    ||                         ||  |
#    ||                         ||  |
#    ||                         ||  |
#    ||                         ||  |
#    ||                         ||  |
#    ||                         ||  /
#    ||_________________________|| /
#    |/_________________________\|/
#       __\_________________/__/|_
#      |_______________________|/ )
#    ________________________    (__
#   /oooo  oooo  oooo  oooo /|   _  )_
#  /ooooooooooooooooooooooo/ /  (_)_(_)
# /ooooooooooooooooooooooo/ /    (o o)
#/C=_____________________/_/    ==\o/==

# Author: M.Salmon

################################################################################
# CONTENTS 
################################################################################
# # MAIN FUNCTION
# Function that manages input and output.
# # FIT GLM FUNCTION
# Function that fits a GLM. 
# # BLOCKS FUNCTION !!! SAME AS IN FARRINGTONFLEXIBLE !!!
# Function that creates the season factor variable for the glm.
# # REFERENCE TIMEPOINTS FUNCTION !!! SAME AS IN FARRINGTONFLEXIBLE !!!
# Function that helps the blocks function as it spots the "reference timepoints"
# of the previous years.
# # GLM FUNCTION
# For now does not do anything other than passing arguments
# Should be the function for checking the time trend.
################################################################################
# END OF CONTENTS
################################################################################
################################################################################
# MAIN FUNCTION
################################################################################


#' The function takes \code{range} values of the surveillance time
#' series \code{sts} and for each time point uses a Bayesian model of the negative binomial family with
#' log link inspired by the work of Noufaily et al. (2012) and of Manitz and H?hle (2014).

#' @param sts sts-object to be analysed. Needs to have a reporting triangle.
#' @param control list with control arguments
#' @param b How many years back in time to include when forming the base counts.
#' @param w Window's half-size, i.e. number of weeks to include before and after the current week in each year.
#' @param range Specifies the index of all timepoints which should be tested. If range is \code{NULL} all possible timepoints are used.
#' @param pastAberrations Boolean indicating whether to include an effect for past outbreaks
#' in a second fit of the model
#' @param verbose Boolean specifying whether to show extra debugging information.
#' @param alpha An approximate (one-sided) \eqn{(1-\alpha)\cdot 100\%}
#' prediction interval is calculated unlike the original method where it was a two-sided interval. The upper limit of this interval
#' i.e. the \eqn{(1-\alpha)\cdot 100\%} quantile serves as an upperbound.
#' @param trend Boolean indicating whether a trend should be included
#' @param noPeriods Number of levels in the factor allowing to use more baseline. If
#' equal to 1 no factor variable is created, the set of reference values is defined as in
#' Farrington et al (1996).
#' @param pastWeeksNotIncluded Number of past weeks to ignore in the calculation.
#' @param delay Boolean indicating whether to take reporting delays into account.
#' @param mc.munu Number of samples for the parameters of the negative binomial distribution
#' when performing Monte Carlo to calculate a threshold
#' @param mc.y Number of samples for observations
#' when performing Monte Carlo to calculate a threshold
#' @param limit54 c(cases,period) is a vector allowing the
#' user to change these numbers.


#' @examples
#' data(salmAllOnset)
#' rangeTest <- 410:412
#' alpha <- 0.05
#' library("INLA")
#' # Control slot for the proposed algorithm with D=0 correction
#' controlNormal <- list(range = rangeTest, b = 4, w = 3,
#'                       pastAberrations = TRUE, mc.munu=10, mc.y=10,
#'                       verbose = FALSE,
#'                       alpha = alpha, trend = TRUE,
#'                       limit54=c(0,50), 
#'                       noPeriods = 10, pastWeeksNotIncluded = 26,
#'                       delay=FALSE)
#' # Control slot for the proposed algorithm with D=10 correction
#' controlDelay <-  list(range = rangeTest, b = 4, w = 3,
#'                       pastAberrations = TRUE, mc.munu=10, mc.y=10,
#'                       verbose = FALSE,
#'                       alpha = alpha, trend = TRUE,
#'                       limit54=c(0,50), 
#'                       noPeriods = 10, pastWeeksNotIncluded = 26,
#'                       delay=TRUE)
#' salm.Normal <- algoDelay(salmAllOnset, controlNormal)

#' salm.delay <- algoDelay(salmAllOnset, controlDelay)
#' plot(salm.Normal)
#' plot(salm.delay)

#' @references A statistical algorithm for the early detection of outbreaks of infectious disease, Farrington, C.P., Andrews, N.J, Beale A.D. and Catchpole, M.A. (1996), J. R. Statist. Soc. A, 159, 547-563.
#' An improved algorithm for outbreak detection in multiple surveillance systems, Noufaily, A., Enki, D.G., Farrington, C.P., Garthwaite, P., Andrews, N.J., Charlett, A. (2012), Statistics in Medicine, published online.

#' @export
algoDelay <- function(sts, control = list(range = NULL, b = 3, w = 3,
                                          mc.munu=100, mc.y=10,
                                          pastAberrations = TRUE, 
                                          verbose = FALSE,
                                          alpha = 0.01, trend = TRUE,
                                          limit54=c(5,4), 
                                          noPeriods = 1, pastWeeksNotIncluded = 26,
                                          delay = TRUE)) {
  #Check if the INLA package is available.
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("The boda function requires the INLA package to be installed.\n",
         "  The package is not available on CRAN, but can be downloaded by calling\n",
         "\tsource(\"http://www.math.ntnu.no/inla/givemeINLA.R\")\n",
         "  as described at http://www.r-inla.org/download in detail.")
  }
  ######################################################################
  # Use special Date class mechanism to find reference months/weeks/days
  ######################################################################  
  if (is.null( sts@epochAsDate)) {
    epochAsDate <- FALSE
  } else {
    epochAsDate <-    sts@epochAsDate
  }
  
  ######################################################################
  # Fetch observed and population
  ######################################################################  
  # Fetch observed
  observed <- observed(sts)
  freq <- sts@freq
  if (epochAsDate) {
    epochStr <- switch( as.character(freq), "12" = "month","52" =    "week",
                        "365" = "day")
  } else { 
    epochStr <- "none"
  }
  
  # Fetch population (if it exists)
  if (!is.null(population(sts))) {
    population <- population(sts) 
  } else {
    population <- rep(1,length(observed))
  }
  
  ######################################################################
  # Fix missing control options
  ######################################################################
  if (is.null(control[["b",exact=TRUE]])) { control$b = 5 }
  
  if (is.null(control[["w", exact = TRUE]])) { control$w = 3 }
  
  if (is.null(control[["range", exact=TRUE]])) {
    control$range <- (freq*(control$b)+control$w +1):dim(observed)[1] 
  }
  if (is.null(control[["pastAberrations",exact=TRUE]])) {control$pastAberrations=TRUE}
  
  if (is.null(control[["verbose",exact=TRUE]]))    {control$verbose=FALSE}
  
  if (is.null(control[["alpha",exact=TRUE]]))        {control$alpha=0.05}
  
  if (is.null(control[["trend",exact=TRUE]]))        {control$trend=TRUE}
  
  
  # No alarm is sounded
  #    if fewer than cases = 5 reports were received in the past period = 4
  #    weeks. limit54=c(cases,period) is a vector allowing the user to change
  #    these numbers
  
  if (is.null(control[["limit54",exact=TRUE]]))    {control$limit54=c(5,4)}
  
  if (is.null(control[["noPeriods",exact=TRUE]])){control$noPeriods=1}
  
  # Use factors in the model? Depends on noPeriods, no input from the user.
  if (control$noPeriods!=1) {
    control$factorsBool=TRUE
  } else {
    control$factorsBool=FALSE
  }

  
  # How many past weeks not to take into account?
  if (is.null(control[["pastWeeksNotIncluded",exact=TRUE]])){ 
    control$pastWeeksNotIncluded=control$w
  }
  
  # Correct for delays?
  if (is.null(control[["delay",exact=TRUE]])) { control$delay = FALSE }
  # Reporting triangle here?
  if (control$delay) {
    if (is.null( sts@control$reportingTriangle$n)) {stop("You have to provide a reporting triangle in control of the sts-object")}
    
    if (!(length(apply(sts@control$reportingTriangle$n,1,sum,na.rm=TRUE))==length(sts@observed)))
    {stop("The reporting triangle number of lines is not the length of the observed slot.")}  
    
    if (!(sum(apply(sts@control$reportingTriangle$n,1,sum,na.rm=TRUE)==sts@observed)==length(sts@observed)))
    {stop("The reporting triangle is wrong: not all cases are in the reporting triangle.")}
  }
  
  # setting for monte carlo integration
  if(is.null(control[["mc.munu",exact=TRUE]])){ control$mc.munu <- 100 }
  if(is.null(control[["mc.y",exact=TRUE]])){ control$mc.y <- 10 }
  ######################################################################
  # Check options
  ######################################################################
  if (!((control$limit54[1] >= 0) && (control$limit54[2] > 0))) {
    stop("The limit54 arguments are out of bounds: cases >= 0 and period > 0.")
  }
  
  # Define objects
  n <- control$b*(2*control$w+1)
  
  
  
  
  
  # loop over columns of sts
  for (j in 1:ncol(sts)) {
    
    #Vector of dates
    if (epochAsDate){
      vectorOfDates <- as.Date(sts@epoch, origin="1970-01-01")
    } else {
      vectorOfDates <- seq_len(length(observed[,j]))
    }    
    
    # Parameters
    b <- control$b
    w <- control$w
    noPeriods <- control$noPeriods
    verbose <- control$verbose
    reportingTriangle <- sts@control$reportingTriangle
    timeTrend <- control$trend
    alpha <- control$alpha
    factorsBool <- control$factorsBool
    pastAberrations <- control$pastAberrations
    glmWarnings <- control$glmWarnings
    delay <- control$delay
    k <- control$k
    verbose <- control$verbose
    pastWeeksNotIncluded <- control$pastWeeksNotIncluded
    mc.munu <- control$mc.munu
    mc.y <- control$mc.y
    # Loop over control$range
    for (k in control$range) {
      
      ######################################################################
      # Prepare data for the glm
      ######################################################################
      dayToConsider <- vectorOfDates[k]  		
      diffDates <- diff(vectorOfDates)
      delay <- control$delay
      
      dataGLM <- algoDelay.data.glm(dayToConsider=dayToConsider, 
                                    b=b, freq=freq, 
                                    epochAsDate=epochAsDate,
                                    epochStr=epochStr,
                                    vectorOfDates=vectorOfDates,w=w,
                                    noPeriods=noPeriods,
                                    observed=observed[,j],population=population,
                                    verbose=verbose,
                                    pastWeeksNotIncluded=pastWeeksNotIncluded,
                                    reportingTriangle=reportingTriangle, 
                                    delay=delay)  
      
      ######################################################################
      # Fit the model 
      ######################################################################      
      argumentsGLM <- list(dataGLM=dataGLM,reportingTriangle=reportingTriangle,
                           timeTrend=timeTrend,alpha=alpha,
                           factorsBool=factorsBool,pastAberrations=pastAberrations,
                           glmWarnings=glmWarnings,
                           verbose=verbose,delay=delay)
      
      model <- do.call(algoDelay.fitGLM, args=argumentsGLM)
      
      argumentsThreshold <- list(model,alpha=alpha,dataGLM=dataGLM,reportingTriangle,
                                 delay=delay,k=k,control=control,mc.munu=mc.munu,mc.y=mc.y)
      threshold <- do.call(algoDelay.threshold,argumentsThreshold)
      
      sts@upperbound[k] <- threshold
      enoughCases <- (sum(observed[(k-control$limit54[2]+1):k,j])
                      >=control$limit54[1])
      sts@alarm[k] <- FALSE
      if (is.na(threshold)){sts@alarm[k] <- NA}
      else {
        if (sts@observed[k]>sts@upperbound[k]) {sts@alarm[k] <- TRUE}
      }
      if(!enoughCases){
        sts@upperbound[k] <- NA
        sts@alarm[k] <- NA
      }
    } #done looping over all time points
  } #end of loop over cols in sts. 
  #   # Add information about score
  #   sts@control$score[,j]     <- score[,j]    
  #   # Add information about trend
  #   sts@control$trend[,j]     <- trend[,j]
  #   
  #   #Add information about predictive distribution
  #   sts@control$pvalue[,j]     <- pvalue[,j]
  #   
  #   # Add information about expected value
  #   sts@control$expected[,j] <- expected[,j]
  #   
  #   # Add information about mean of the negbin distribution of the observation
  #   sts@control$mu0Vector[,j] <- mu0Vector[,j]
  #   
  #   # Add information about overdispersion
  #   sts@control$phiVector[,j] <- phiVector[,j]
  #   
  #   # Add information about time trend
  #   sts@control$trendVector[,j] <- trendVector[,j]    
  
  #Done
  
  return(sts[control$range,]) 
}
################################################################################
# END OF MAIN FUNCTION
################################################################################

################################################################################
# REFERENCE TIME POINTS FUNCTION
################################################################################
algoDelay.referencetimepoints <- function(dayToConsider,b=control$b,freq=freq,epochAsDate,epochStr){
  
  
  if (epochAsDate) {
    referenceTimePoints <- as.Date(seq(as.Date(dayToConsider, 
                                               origin="1970-01-01"),
                                       length=(b+1), by="-1 year"))
  } else {
    referenceTimePoints <- seq(dayToConsider, length=(b+1),by=-freq)
    
    if (referenceTimePoints[b+1]<=0){
      warning("Some reference values did not exist (index<1).")
    }
    
  }
  
  if (epochStr == "week") {
    
    # get the date of the Mondays/Tuesdays/etc so that it compares to 
    # the reference data
    # (Mondays for Mondays for instance)
    
    # Vectors of same days near the date (usually the same week)
    # dayToGet
    dayToGet <- as.numeric(format(dayToConsider, "%w"))
    actualDay <- as.numeric(format(referenceTimePoints, "%w")) 
    referenceTimePointsA <- referenceTimePoints -    
      (actualDay 
       - dayToGet)
    
    # Find the other "same day", which is either before or after referenceTimePoints
    
    referenceTimePointsB <- referenceTimePointsA + ifelse(referenceTimePointsA>referenceTimePoints,-7,7)
    
    
    # For each year choose the closest Monday/Tuesday/etc
    # The order of referenceTimePoints is NOT important
    
    AB <- cbind(referenceTimePointsA,referenceTimePointsB)
    ABnumeric <- cbind(as.numeric(referenceTimePointsA),as.numeric(referenceTimePointsB))
    distMatrix <- abs(ABnumeric-as.numeric(referenceTimePoints))
    idx <- (distMatrix[,1]>distMatrix[,2])+1
    referenceTimePoints <- as.Date(AB[cbind(1:dim(AB)[1],idx)],origin="1970-01-01")
    
  }
  
  return(referenceTimePoints)
}
################################################################################
# END OF REFERENCE TIME POINTS FUNCTION
################################################################################

################################################################################
# BLOCKS FUNCTION !!! SAME AS IN FARRINGTONFLEXIBLE !!!
################################################################################
blocks <- function(referenceTimePoints,vectorOfDates,freq,dayToConsider,b,w,p,
                   epochAsDate) {
  
  ## INPUT
  # freq: are we dealing with daily/weekly/monthly data?
  
  # b: how many years to go back in time
  
  # w: half window length around the reference timepoints
  
  # p: number of noPeriods one wants the year to be split into
  
  ## VECTOR OF ABSOLUTE NUMBERS
  # Very useful to write the code!
  
  vectorOfAbsoluteNumbers <- seq_len(length(vectorOfDates))
  
  
  # logical vector indicating where the referenceTimePoints 
  # are in the vectorOfDates
  referenceTimePointsOrNot <- vectorOfDates %in%    referenceTimePoints
  
  ## VECTOR OF FACTORS
  vectorOfFactors <- rep(NA,length(vectorOfDates))
  
  ## SETTING THE FACTORS
  # Current week
  if (epochAsDate==FALSE){
    now <- which(vectorOfDates==dayToConsider)
  } else {
    now <- which(vectorOfDates==as.Date(dayToConsider))
  }
  
  
  
  vectorOfFactors[(now-w):now]    <- p
  
  # Reference weeks
  
  referenceWeeks <- rev(as.numeric(
    vectorOfAbsoluteNumbers[referenceTimePointsOrNot=='TRUE'])) 
  
  
  for (i in 1:b) {
    
    # reference week
    
    refWeek <- referenceWeeks[i+1]
    
    vectorOfFactors[(refWeek-w):(refWeek+w)] <- p
    
    # The rest is only useful if one wants factors, otherwise only have 
    # reference timepoints like in the old Farrington method
    
    if (p!=1){
      # Number of time points to be shared between vectors
      period <- referenceWeeks[i] - 2 * w - 1 - refWeek
      
      # Check that p is not too big
      if (period < (p-(2*w+1))){stop('Number of factors too big!')}
      
      # Look for the length of blocks
      
      lengthOfBlocks <- period %/% (p-1)
      rest <- period %% (p-1)
      
      vectorLengthOfBlocks <- rep(lengthOfBlocks,p-1)
      
      # share the rest of the Euclidian division among the first blocks
      
      add <- seq_len(rest)
      vectorLengthOfBlocks[add] <-    vectorLengthOfBlocks[add]+1
      
      # slight transformation necessary for the upcoming code with cumsum
      vectorLengthOfBlocks <- c(0,vectorLengthOfBlocks)
      
      # fill the vector
      
      for (j in 1:(p-1)) { 
        vectorOfFactors[(refWeek+w+1+cumsum(vectorLengthOfBlocks)[j]):
                          (refWeek+w+1+cumsum(vectorLengthOfBlocks)[j+1]-1)]<-j
      }
    }
  }
  
  ## DONE!
  
  return(vectorOfFactors) #indent
}

################################################################################
# END OF BLOCKS FUNCTION
################################################################################


################################################################################
# FIT GLM FUNCTION
################################################################################

algoDelay.fitGLM <- function(dataGLM,reportingTriangle,alpha,
                             timeTrend,factorsBool,delay,pastAberrations,
                             glmWarnings,verbose,...) {
  
  # Model formula depends on whether to include a time trend or not.
  
  theModel <- formulaGLMDelay(timeBool=timeTrend,factorsBool,delay,outbreak=FALSE)
  # Fit it -- this is slow. An improvement would be to use glm.fit here.
  # This would change the syntax, however.
  E <- max(0,mean(dataGLM$response, na.rm=TRUE))
  link=1
  model <- INLA::inla(as.formula(theModel),data=dataGLM,
                family='nbinomial',E=E,
                control.predictor=list(compute=TRUE,link=link),
                control.compute=list(cpo=TRUE,config=TRUE),
                control.inla = list(int.strategy = "grid",dz=1,diff.logdens = 10),
                control.family = list(hyper = list(theta = list(prior = "normal", param = c(0, 0.001)))))
  
  if (pastAberrations){
    # if we have failures => recompute those manually
    #if (sum(model$cpo$failure,na.rm=TRUE)!=0){
    #   model <- inla.cpo(model)
    #}
    # Calculate the mid p-value
    vpit <- model$cpo$pit
    vcpo <- model$cpo$cpo
    midpvalue <- vpit - 0.5*vcpo
    # Detect the point with a high mid p-value
    
    #hist(midpvalue)
    #print(summary(model))
    # outbreakOrNot <- midpvalue
    #outbreakOrNot[midpvalue  <= (1-alpha)] <- 0
    outbreakOrNot <- ifelse(midpvalue  > (1-alpha), 1, 0) 
    outbreakOrNot[is.na(outbreakOrNot)] <- 0# FALSE
    outbreakOrNot[is.na(dataGLM$response)] <- 0#FALSE
    
    # Only recompute the model if it will bring something!
    if (sum(outbreakOrNot)>0){
      dataGLM <- cbind(dataGLM,outbreakOrNot)
      theModel <- formulaGLMDelay(timeBool=timeTrend,factorsBool,delay,outbreak=TRUE)
      
      model <- INLA::inla(as.formula(theModel),data=dataGLM,
                    family='nbinomial',E=E,
                    control.predictor=list(compute=TRUE,link=link),
                    control.compute=list(cpo=FALSE,config=TRUE),
                    control.inla = list(int.strategy = "grid",dz=1,diff.logdens = 10),
                    control.family = list(hyper = list(theta = list(prior = "normal", param = c(0, 0.001)))))
      
      # if we have failures => recompute those manually
      #  if (sum(model$cpo$failure,na.rm=TRUE)!=0){model <- inla.cpo(model)}
      vpit <- model$cpo$pit
      vcpo <- model$cpo$cpo
      midpvalue <- vpit - 0.5*vcpo 
    }
  }
  
  
  return(model)
}
################################################################################
# END OF FIT GLM FUNCTION
################################################################################

################################################################################
# THRESHOLD FUNCTION
################################################################################
algoDelay.threshold <- function(model, mc.munu,mc.y,alpha,
                                delay,k,control,dataGLM,reportingTriangle,...) {
  
  E <- max(0,mean(dataGLM$response, na.rm=TRUE))
  # Sample from the posterior
  jointSample <- INLA::inla.posterior.sample(mc.munu,model,hyper.user.scale = FALSE)

  # take variation in size hyperprior into account by also sampling from it
  theta <- t(sapply(jointSample, function(x) x$hyperpar))

  if (delay){
    mu_Tt <- numeric(mc.munu)
    N_Tt <- numeric(mc.munu*mc.y)
    
    # Maximal delay + 1
    Dmax0 <- ncol(as.matrix(reportingTriangle$n))
    # The sum has to be up to min(D,T-t). This is how we find the right indices.
    loopLimit <- min(Dmax0,which(is.na(as.matrix(reportingTriangle$n)[k,]))-1,na.rm=TRUE)

    # Find the ntd and sum
    for (d in 1:loopLimit)
    {
      if(sum(dataGLM$response[dataGLM$delay==d],na.rm=TRUE)!=0){
      mu_Tt <- mu_Tt + exp(t(sapply(jointSample, function(x) x$latent[[nrow(dataGLM)-Dmax0+d]])))
      }

    }

    N_Tt <- rnbinom(size=theta,mu=E*mu_Tt,n=mc.munu*mc.y)
    N_Tt <- N_Tt[is.na(N_Tt)==FALSE]
    qi <- quantile(N_Tt, probs=(1-alpha), type=3, na.rm=TRUE)
    # with no delay this is similar to boda.
  } else {
    mT1 <- exp(t(sapply(jointSample, function(x) x$latent[[nrow(dataGLM)]])))
    
    #Draw (mc.munu \times mc.y) responses. 
    N_Tt <- rnbinom(n=mc.y*mc.munu,size=exp(theta),mu=E*mT1)
    
    qi <- quantile(N_Tt, probs=(1-alpha), type=3, na.rm=TRUE)
  }

  return(as.numeric(qi))
  
  
}
################################################################################
# END OF THRESHOLD GLM FUNCTION
################################################################################

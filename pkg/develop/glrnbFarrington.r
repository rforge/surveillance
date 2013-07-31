#     ____________________________
#    |\_________________________/|\
#    ||                         || \
#    ||    glrnb with glm.nb    ||  \
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

# Version of the 05.07.2013
# M.Salmon with code from algo_glrnb (M. Höhle, V. Wimmer)

glrnbFarrington <- function(sts, control = list(range = NULL, b = 3, w = 3,
                                      reweight = TRUE, 
                                      weightsThreshold = 2.58,
                                      glmWarnings = TRUE,
                                      trend = TRUE,
                                      pThresholdTrend=1,                                     
                                      fitFun="fitGLM.nb",
                                      populationOffset = FALSE, 
                                      pastWeeksNotIncluded = 26, 
									  sinCos = TRUE,
									  c.ARL = 5, Mtilde = 1,
									  M =-1, change="intercept",
									  theta=NULL,dir=c("inc","dec"),
									  ret=c("cases","value"))) {

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
	# How many years to go back in time?
	if (is.null(control[["b",exact=TRUE]])) { control$b = 5 }

	# Half-window length
	if (is.null(control[["w", exact = TRUE]])) { control$w = 3 }
	# How many past weeks not to take into account?
	if (is.null(control[["pastWeeksNotIncluded",exact=TRUE]])){ 
	control$pastWeeksNotIncluded=control$w
	}

	# Range of time points to be evaluated
	if (is.null(control[["range", exact=TRUE]])) {
	control$range <- ((freq*(control$b)+control$w +1 + control$pastWeeksNotIncluded):length(observed)) 
	}


	# Reweighting past outbreaks?
	if (is.null(control[["reweight",exact=TRUE]])) {control$reweight=TRUE}
	# With which threshold?
	if (is.null(control[["weightsThreshold",exact=TRUE]])) {
	control$weightsThreshold=2.58
	}


	# Printing warning from glm.fit?
	if (is.null(control[["glmWarnings",exact=TRUE]]))  {control$glmWarnings=TRUE}


	# Include a time trend when possible?
	if (is.null(control[["trend",exact=TRUE]]))    {control$trend=TRUE}

	# Which pvalue for the time trend to be significant?
	if (is.null(control[["pThresholdTrend",exact=TRUE]])){
	control$pThresholdTrend=0.05}  

	# Use a population offset in the model?
	if (is.null(control[["populationOffset",exact=TRUE]])) {
	control$populationOffset=FALSE
	}
	# Use a sine cosine in the model?
	if (is.null(control[["sinCos",exact=TRUE]])) {
	control$sinCos=TRUE
	}  


	# Which function to use?
	# Only one possibility at the moment  
	if (is.null(control[["fitFun",exact=TRUE]]))   {
	control$fitFun="fitGLM.nb"
	} else {
	control$fitFun <- match.arg(control$fitFun, c(
	  "fitGLM.nb"))
	}

	# Which method for calculating the threshold?
	if (is.null(control[["thresholdMethod",exact=TRUE]]))
	{ control$thresholdMethod="delta"}

	# Set the default values for the glrnb if not yet set
	if(is.null(control[["c.ARL",exact=TRUE]]))
	control$c.ARL <- 5
	if(is.null(control[["change",exact=TRUE]]))
	control$change <- "intercept"
	if(is.null(control[["Mtilde",exact=TRUE]]))
	control$Mtilde <- 1
	if(is.null(control[["M",exact=TRUE]]))
	control$M <- -1
	if(is.null(control[["dir",exact=TRUE]]))
	control$dir <- "inc"
	if(is.null(control[["ret",exact=TRUE]]))
	control$ret <- "value"
	if(!is.null(control[["theta",exact=TRUE]])) {
	if(control[["theta",exact=TRUE]] == 1) {
	  stop("Error: theta has to be larger than 1!")
	}
	} 


    ######################################################################
    # Initialize the necessary vectors
    ######################################################################
    # Vector for time trend
    trend <- matrix(data = 0, nrow = length(control$range)+control$pastWeeksNotIncluded, ncol = ncol(sts))
	sts@control$trend<- trend
	
    # Vector for expected count
    expected <- matrix(data = 0, nrow = length(control$range)+control$pastWeeksNotIncluded, ncol = ncol(sts))
	sts@control$expected <- expected

    # Vector for overdispersion phi (from glm)
    phiVector <- matrix(data = 0, nrow = length(control$range)+control$pastWeeksNotIncluded, ncol = ncol(sts))
	sts@control$phiVector <- phiVector
	
    # Vector for time trend (from glm)
    trendVector <- matrix(data = 0, nrow = length(control$range)+control$pastWeeksNotIncluded, ncol = ncol(sts))
	sts@control$trendVector <- trendVector
	
    # loop over columns of sts
    for (j in 1:ncol(sts)) {
	
		################################################
		#Extract the important parts from the arguments
		################################################

		control$dir <- match.arg(control$dir, c("inc","dec"))
		dir <- ifelse(control$dir=="inc",1,-1)
		control$ret <- match.arg(control$ret, c("value","cases"))
		ret <- pmatch(control$ret,c("value","cases"))
		mod <- list()
        
    	#Vector of dates
		if (epochAsDate){
			vectorOfDates <- as.Date(sts@epoch, origin="1970-01-01")
		} else {
			vectorOfDates <- seq_len(length(observed[,j]))
		}    
		
		#Setup counters for the progress
		
		# doneidx gives the number of timepoints between beginning of monitoring and last alarm
		# doneidx is used in the condition of the while loop
		# and to get the indices for the date for instance
		doneidx <- 0
		# limit for the while loop
		noOfTimePoints <- length(control$range) + control$pastWeeksNotIncluded	
		# start value for the cusum algorithm
		xm10 <- 0
		
		# counts the number of alarms given since beginning of monitoring
		noofalarms <- 0
		
		# get the vector of counts
		observed <- sts@observed[,j]
		# get the counts that will be monitored
		x <- observed[(min(control$range) - control$pastWeeksNotIncluded):max(control$range)]
		
		# create upperbound and alarm vectors
		upperbound <- rep(NA,noOfTimePoints)
		alarm <- rep(FALSE,noOfTimePoints)

		# start the loop
        while (doneidx < noOfTimePoints) {

			# day at which to (re)start monitoring
			dayToConsider <- vectorOfDates[(min(control$range) - control$pastWeeksNotIncluded):max(control$range)][1 + doneidx]

			# Prepare data for fitting the glm
			dataGLM <- glrnb.data.glm(dayToConsider, b=control$b, 
										 vectorOfDates,w=control$w,
										 observed,population,
										 verbose=control$verbose,
										 epochAsDate,epochStr,freq)
										 
			
			# Prepare data for getting in-control parameters (mean and overdispersion)
			dataPred <- glrnb.pred.glm(dayToConsider,timeN=max(control$range),vectorOfDates,observed,population,freq,timeOne=max(dataGLM$wtime))					
			
			# Fit the model and calculate predictions
			finalModel <- algo.glmNB(dataGLM,timeTrend=control$trend,populationOffset=control$populationOffset,
			                                   sinCos=control$sinCos,
					                          reweight=control$reweight,weightsThreshold=control$weightsThreshold,
											  pThresholdTrend=control$pThresholdTrend,b=control$b,
					                          fitFun=control$fitFun,
											  glmWarnings=control$glmWarnings,epochAsDate,
					                          dayToConsider,dataPred=dataPred,verbose=control$verbose)
			
			# Get mu0 and alpha from the fitted model for the CUSUM
			mu0 <- finalModel$pred$fit

			expected[((1 + doneidx):noOfTimePoints),j] <- finalModel$pred$fit
			trend[((1 + doneidx):noOfTimePoints),j] <- rep(finalModel$doTrend,length(finalModel$pred$fit))
			trendVector[((1 + doneidx):noOfTimePoints),j] <- rep(finalModel$coeffTime,length(finalModel$pred$fit))
			phiVector[((1 + doneidx):noOfTimePoints),j] <- rep(finalModel$phi,length(finalModel$pred$fit))
			
			alpha <- finalModel$alpha

			if (control$change == "intercept") {
				if (is.null(control[["theta",exact=TRUE]])) {
					if (alpha == 0) { #poisson

						if (control$M > 0 ){ # window limited
						  
							res <- .C("glr_cusum_window",as.integer(x),as.double(mu0),length(x),as.integer(control$M),as.integer(control$Mtilde),as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),cases=as.double(numeric(length(x))),as.integer(dir),as.integer(ret),PACKAGE="surveillance")
						} 
						else { # standard

							res <- .C("glr_cusum",as.integer(x),as.double(mu0),length(x),as.integer(control$Mtilde),as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),cases=as.double(numeric(length(x))),as.integer(dir),as.integer(ret),PACKAGE="surveillance")

							}
					} else { #negbin
						res <- .C("glr_nb_window",x=as.integer(x),mu0=as.double(mu0),alpha=as.double(alpha),lx=length(x),Mtilde=as.integer(control$Mtilde),M=as.integer(control$M),c.ARL=as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),dir=as.integer(dir),PACKAGE="surveillance")
					}
				} else { ###################### !is.null(control$theta)
					if (alpha == 0) { #poisson

						res <- .C("lr_cusum",x=as.integer(x),mu0=as.double(mu0),lx=length(x),as.double(control$theta),c.ARL=as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),cases=as.double(numeric(length(x))),as.integer(ret),PACKAGE="surveillance")

					} else { #negbin
						res <- .C("lr_cusum_nb",x=as.integer(x),mu0=as.double(mu0),alpha=as.double(alpha),lx=length(x),as.double(control$theta),c.ARL=as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),cases=as.double(numeric(length(x))),as.integer(ret),PACKAGE="surveillance")

					}
				}
			} else { ################### Epidemic chart #######################
				if (control$change == "epi") {
					if (alpha == 0) { #pois
						res <- .C("glr_epi_window",as.integer(x),as.double(mu0),length(x),as.integer(control$Mtilde),as.integer(control$M),as.double(xm10),as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),PACKAGE="surveillance")
					} else {
						res <- .C("glr_nbgeneral_window",as.integer(x),as.double(mu0),alpha=as.double(alpha),lx=length(x),Mtilde=as.integer(control$Mtilde),M=as.integer(control$M),xm10=as.double(xm10),c.ARL=as.double(control$c.ARL),N=as.integer(0),val=as.double(numeric(length(x))),dir=as.integer(dir),PACKAGE="surveillance")
					}
				}
			}	


	
			#In case an alarm found log this and reset the chart at res$N+1
			if (res$N < length(x)) {
				upperbound[1:res$N + doneidx]  <- either(ret == 1, res$val[1:res$N] ,res$cases[1:res$N])	
				alarm[res$N + doneidx] <- TRUE
				#Chop & get ready for next round
				xm10 <- x[res$N] #put start value x_0 to last value
				x <- x[-(1:res$N)] ; 
				noofalarms <- noofalarms + 1

			}


    doneidx <- doneidx + res$N

        }#done looping over all time points
  
	# fix of the problem that no upperbound-statistic is returned after 
	#last alarm
	upperbound[(doneidx-res$N+1):length(upperbound)] <- either(ret == 1, res$val, res$cases)
	
		
	#fix of the problem that no upperbound-statistic is returned 
	#in case of no alarm

	 if (noofalarms == 0) {
		upperbound <- either(ret==1, res$val, res$cases)
	 }

	sts@upperbound[(min(control$range) - control$pastWeeksNotIncluded):max(control$range),j] <- upperbound
	sts@alarm[(min(control$range) - control$pastWeeksNotIncluded):max(control$range),j] <- alarm
	# ensure upper bound is positive and not NaN
	sts@upperbound[is.na(sts@upperbound)] <- 0
	sts@upperbound[sts@upperbound < 0] <- 0

		# Add information about trend
		# sts@control$trend[,j]     <- trend[,j]
		
		
		# Add information about expected value

		sts@control$expected[,j] <- expected[,j]
		sts@control$trend[,j] <- trend[,j]
		sts@control$trendVector[,j] <- trendVector[,j]
		sts@control$phiVector[,j] <- phiVector[,j]
		
		# Add information about overdispersion
		# sts@control$phiVector[,j] <- phiVector[,j]
		
		# Add information about time trend
		# sts@control$trendVector[,j] <- trendVector[,j]    
    } #end of loop over cols in sts.
    #Done

    return(sts[(min(control$range) - control$pastWeeksNotIncluded):max(control$range)]) 
}

################################################################################
# END OF MAIN FUNCTION
################################################################################

################################################################################
# Small helper function
################################################################################

  either <- function(cond, whenTrue, whenFalse) { if (cond) return(whenTrue) else return(whenFalse) }
  
################################################################################
# End of small helper function
################################################################################
################################################################################
# GLM FUNCTION
################################################################################

algo.glmNB <- function(dataGLM,timeTrend,populationOffset,sinCos,
                                reweight,weightsThreshold,pThresholdTrend,b,
								fitFun,glmWarnings,epochAsDate,
								dayToConsider,dataPred,verbose) {

	arguments <- list(dataGLM=dataGLM,
					  timeTrend=timeTrend,
					  populationOffset=populationOffset,
					  sinCos=sinCos,reweight=reweight,
					  weightsThreshold=weightsThreshold,glmWarnings=glmWarnings,
					  verbose=verbose,control=control)

	model <- do.call(fitFun, args=arguments)

	#Stupid check to pass on NULL values from the algo.farrington.fitGLM proc.
	if (is.null(model)) return(model)

	######################################################################
	#Time trend
	######################################################################

	#Check whether to include time trend, to do this we need to check whether
	#1) wtime is signifcant at the 95lvl
	#2) the predicted value is not larger than any observed value
	#3) the historical data span at least 3 years.
	doTrend <- NULL
	
	# if model converged with time trend 
	if ("wtime" %in% names(coef(model))){

		# get the prediction for range
		pred <- predict.glm(model,newdata=dataPred,se.fit=TRUE,type="response")
									 
		# check if three criterion ok
		
		 #is the p-value for the trend significant (0.05) level
		significant <- (summary.glm(model)$coefficients["wtime",4] < pThresholdTrend)

		#have to use at least three years of data to allow for a trend
		atLeastThreeYears <- (b>=3)
		#no horrible predictions

		noExtrapolation <- (sum(pred$fit > max(dataGLM$response))==0)
			
		#All 3 criteria have to be met in order to include the trend. Otherwise
		#it is removed. Only necessary to check this if a trend is requested.
		doTrend <- (atLeastThreeYears && significant && noExtrapolation) 

		# if not then refit
		if (doTrend==FALSE) {
			arguments$timeTrend=FALSE
			model <- do.call(fitFun, args=arguments) 

		} 
	} else {
		
		doTrend <- FALSE
	}

	
	#done with time trend
	######################################################################

	######################################################################
	# Calculate prediction                                              #
	######################################################################
	#Predict values

	pred <- predict.glm(model,newdata=dataPred,se.fit=TRUE,type="response")
	coeffTime=ifelse(doTrend,summary.glm(model)$coefficients["wtime",1],NA)
	finalModel <- list (pred,doTrend,coeffTime,model$theta,model$phi)		
	names(finalModel) <- c("pred","doTrend","coeffTime","alpha","phi")
    return(finalModel)        

}




################################################################################
# END OF GLM FUNCTION
################################################################################

################################################################################
# DATA GLM FUNCTION
################################################################################

# Comme dans farringtonFlexible!
glrnb.data.glm <- function(dayToConsider, b, 
                                     vectorOfDates,w,
									 observed,population,
									 verbose,
									 epochAsDate,epochStr,freq){
	firstReferencetimepoint <- firstReferencetimepoint(dayToConsider,b=b,freq=freq,epochAsDate,epochStr)	

	blockIndexes <- which(vectorOfDates %in% seq(firstReferencetimepoint,(dayToConsider-1),by=diff(vectorOfDates)[1]))
	
	if (w>0){
	blockIndexes <- c(seq(blockIndexes[1]-w,blockIndexes[1]-1,by=1),blockIndexes)
	}

	
	response <- observed[blockIndexes]
	pop <- population[blockIndexes]
	wtime <- (as.numeric(vectorOfDates[blockIndexes])-
								as.numeric(vectorOfDates[blockIndexes][1]))/as.numeric(diff(vectorOfDates))[1] +1


	trigo.t <- sin(2*pi*wtime/freq)+ cos(2*pi*wtime/freq)
	
	dataGLM <- data.frame(response=response,wtime=wtime,population=pop,
						 vectorOfDates=vectorOfDates[blockIndexes],
						 trigo.t=trigo.t)
	
	return(dataGLM)

}
################################################################################
# END OF DATA GLM FUNCTION
################################################################################



################################################################################
# FORMULA FUNCTION
################################################################################
# Function for writing the good formula depending on timeTrend,
# populationOffset and sinCos

formulaGLM.glrnb <- function(populationOffset=FALSE,timeBool=TRUE,sinCos=TRUE){
  # Description
  # Args:
  #   populationOffset: ---
  # Returns:
  #   Vector of X
  
  # Smallest formula
  formulaString <- "response ~ 1"

  # With time trend?
  if (timeBool){
  formulaString <- paste(formulaString,"+wtime",sep ="")}

  # With population offset?

  if(populationOffset){
  formulaString <- paste(formulaString,"+offset(log(population))",sep ="")}
  # With sinus cosinus?

  if(sinCos){
  formulaString <- paste(formulaString,"+ trigo.t",sep ="")}


  # Return formula as a string
  return(formulaString)
}
################################################################################
# END OF FORMULA FUNCTION
################################################################################

################################################################################
# REFERENCE TIME POINTS FUNCTION
################################################################################
firstReferencetimepoint <- function(dayToConsider,b=b,freq=freq,epochAsDate,epochStr){

	if(epochAsDate){
		# take the date b years before the "current" timepoint
		d <- as.POSIXlt(as.Date(dayToConsider, origin="1970-01-01"))
		d$year <- d$year-b
		firstReferencetimepoint <- as.Date(d)
		# if we have weekly data find the closest day on the same weekday
		if (epochStr == "week") {

			# get the date of the Mondays/Tuesdays/etc so that it compares to 
			# the reference data
			# (Mondays for Mondays for instance)

			# Vectors of same days near the date (usually the same week)
			# dayToGet

			dayToGet <- as.numeric(format(dayToConsider, "%w"))

			actualDay <- as.numeric(format(firstReferencetimepoint, "%w")) 
			firstReferencetimepointA <- firstReferencetimepoint -    
				(actualDay 
				 - dayToGet)

			# Find the other "same day", which is either before or after referenceTimePoints

			firstReferencetimepointB <- firstReferencetimepointA + ifelse(firstReferencetimepointA>firstReferencetimepoint,-7,7)


			# Choose the closest Monday/Tuesday/etc

			AB <- c(firstReferencetimepointA,firstReferencetimepointB)
			ABnumeric <- c(as.numeric(firstReferencetimepointA),as.numeric(firstReferencetimepointB))
			distMatrix <- abs(ABnumeric-as.numeric(firstReferencetimepoint))
			idx <- (distMatrix[1]>distMatrix[2])+1
			firstReferencetimepoint <- as.Date(AB[idx],origin="1970-01-01")

		}
	} else {
		firstReferencetimepoint <- dayToConsider - b*freq

		if (firstReferencetimepoint<=0){
			warning("Some reference values did not exist (index<1).")
		}

	}
	


	return(firstReferencetimepoint)
	}
################################################################################
# END OF REFERENCE TIME POINTS FUNCTION
################################################################################


################################################################################
# FIT GLM FUNCTION
################################################################################

fitGLM.nb <- function(dataGLM,
timeTrend,populationOffset,sinCos,reweight,weightsThreshold,glmWarnings,verbose,control,...) {
    
    # Model formula depends on whether to include a time trend or not.

    theModel <- formulaGLM.glrnb (populationOffset,timeBool=timeTrend,sinCos)
    
    # Fit it -- this is slow. An improvement would be to use glm.fit here.
    # This would change the syntax, however.
    if (glmWarnings) {
        model <- glm.nb(formula(theModel),data=dataGLM,link=log)
    } else {
        model <- suppressWarnings(glm.nb(formula(theModel),data=dataGLM,link=log))
    }                         
    #Check convergence - if no convergence we return empty handed.
    
    if (!model$converged) {
        #Try without time dependence
     
        if (timeTrend) {
			theModel <- formulaGLM.glrnb (populationOffset,timeBool=F,sinCos)
			if (glmWarnings) {
				model <- glm.nb(as.formula(theModel), data=dataGLM,
												link=log)
			} else {
				model <- suppressWarnings(glm.nb(as.formula(theModel), data=dataGLM,
																					link=log))
			}
			if (verbose) {cat("Warning: No convergence with timeTrend -- trying without.\n")}
        }
        
        if (!model$converged) {
        if (verbose) {cat("Warning: No convergence in this case.\n")}
        if (verbose) {print(dataGLM[,c("response","wtime"),exact=TRUE])}
        return(NULL)
        }
    }
    
    #Overdispersion parameter phi
    
    phi <- max(summary(model)$dispersion,1)
    
    #In case reweighting using Anscome residuals is requested
    
    if (reweight) {
        s <- anscombe.residuals(model,phi)
        omega <- algo.farrington.assign.weights(s,weightsThreshold)
        if (glmWarnings) {
        model <- glm.nb(as.formula(theModel),data=dataGLM,
                                        link=log,
                                        weights=omega) 
        } else {
	    model <- suppressWarnings(glm.nb(as.formula(theModel),data=dataGLM,
                                                                        link=log,
                                                                        weights=omega)) 
        }
        
        #Here, the overdispersion often becomes small, so we use the max
        #to ensure we don't operate with quantities less than 1.
        phi <- max(summary(model)$dispersion,1)
    } # end of refit.
    
    
    #Add wtime, response and phi to the model
    model$phi <- phi
	model$theta <- model$theta
    model$wtime <- dataGLM$wtime
    model$response <- dataGLM$response
    model$population <- dataGLM$population
    if (reweight) {
	model$weights <- omega
    } else{
    model$weights <- NA
    }
    #Done
    return(model)

}
################################################################################
# END OF FIT GLM FUNCTION
################################################################################



################################################################################
# DATA PRED FUNCTION
################################################################################

# Comme dans farringtonFlexible!
glrnb.pred.glm <- function(dayToConsider,timeN,vectorOfDates,observed,population,freq,timeOne){

	blockIndexes <- (which(vectorOfDates==dayToConsider):timeN)

	response <- observed[blockIndexes]
	pop <- population[blockIndexes]
	wtime <- (as.numeric(vectorOfDates[blockIndexes])-
								as.numeric(vectorOfDates[blockIndexes][1]))/as.numeric(diff(vectorOfDates))[1] +timeOne+1


	trigo.t <- sin(2*pi*wtime/freq)+ cos(2*pi*wtime/freq)
	
	dataPred <- data.frame(response=response,wtime=wtime,population=pop,
						 vectorOfDates=vectorOfDates[blockIndexes],
						 trigo.t=trigo.t)
	
	return(dataPred)

}
################################################################################
# END OF DATA GLM FUNCTION
################################################################################
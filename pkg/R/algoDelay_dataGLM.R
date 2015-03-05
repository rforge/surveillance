################################################################################
# DATA GLM FUNCTION
################################################################################
algoDelay.data.glm <- function(dayToConsider, b, freq, 
                               epochAsDate,epochStr,
                               vectorOfDates,w,noPeriods,
                               observed,population,
                               verbose,pastWeeksNotIncluded,reportingTriangle,delay){
  
  
  # Identify reference time points
  
  # Same date but with one year, two year, etc, lag
  # b+1 because we need to have the current week in the vector
  referenceTimePoints <- algoDelay.referencetimepoints(dayToConsider,b=b,
                                                       freq=freq,
                                                       epochAsDate=epochAsDate,
                                                       epochStr=epochStr
  )
  
  if (sum((vectorOfDates %in% min(referenceTimePoints)) == rep(FALSE,length(vectorOfDates))) == length(vectorOfDates)){
    warning("Some reference values did not exist (index<1).")
  }
  
  
  # Create the blocks for the noPeriods between windows (including windows)
  # If noPeriods=1 this is a way of identifying windows, actually.
  
  blocks <- blocks(referenceTimePoints,vectorOfDates,epochStr,dayToConsider,
                   b,w,noPeriods,epochAsDate)
  
  # Here add option for not taking the X past weeks into account
  # to avoid adaptation of the model to emerging outbreaks
  blocksID <- blocks
  
  
  # Extract values for the timepoints of interest only
  
  blockIndexes <- which(is.na(blocksID)==FALSE) 
  
  
  # Time
  
  # if epochAsDate make sure wtime has a 1 increment
  if (epochAsDate){
    wtime <- (as.numeric(vectorOfDates[blockIndexes])-
                as.numeric(vectorOfDates[blockIndexes][1]))/as.numeric(diff(vectorOfDates))[1]
  } else {
    wtime <-     as.numeric(vectorOfDates[blockIndexes])
  }
  
  # Factors
  seasgroups <- as.factor(blocks[blockIndexes])
  
  # Observed
  response <- as.numeric(observed[blockIndexes])
  response[length(response)] <- NA
  # Population
  pop <- population[blockIndexes]
  
  if (verbose) { print(response)}
  
  # Delays
  
  delays <- as.factor(0:(dim(reportingTriangle$n)[2]-1))
  
  # If the delays are not to be taken into account it is like farringtonFlexible
  if (!delay) {
    dataGLM <- data.frame(response=response,wtime=wtime,population=pop,
                          seasgroups=seasgroups,vectorOfDates=vectorOfDates[blockIndexes])
    
    dataGLM$response[(nrow(dataGLM)-pastWeeksNotIncluded):nrow(dataGLM)] <- NA
  }
  # If the delays are to be taken into account we need a bigger dataframe
  else {
    # Take the subset of the reporting triangle corresponding to the timepoints used for fitting the model
    reportingTriangleGLM <- reportingTriangle$n[rownames(reportingTriangle$n) %in% as.character(vectorOfDates[blockIndexes]),]
    
    # All vectors of data will be this long: each entry will correspond to one t and one d
    lengthGLM <- dim(reportingTriangleGLM)[2]*dim(reportingTriangleGLM)[1]
    
    # Create the vectors for storing data
    responseGLM <- numeric(lengthGLM)
    wtimeGLM <- numeric(lengthGLM)
    seasgroupsGLM <- numeric(lengthGLM)
    popGLM <- numeric(lengthGLM)
    vectorOfDatesGLM <- numeric(lengthGLM)
    delaysGLM <- numeric(lengthGLM) 
    
    # Fill them D by D
    D <- dim(reportingTriangleGLM)[2]
    for (i in (1:dim(reportingTriangleGLM)[1])){
      vectorOfDatesGLM[((i-1)*D+1):(i*D)] <- rep(vectorOfDates[blockIndexes][i],D)
      wtimeGLM[((i-1)*D+1):(i*D)] <- rep(wtime[i],D)
      popGLM[((i-1)*D+1):(i*D)] <- rep(pop[i],D)
      seasgroupsGLM[((i-1)*D+1):(i*D)] <- rep(seasgroups[i],D)
      responseGLM[((i-1)*D+1):(i*D)] <- reportingTriangleGLM[i,]
      delaysGLM[((i-1)*D+1):(i*D)] <- 0:(D-1)
      
    }
    
    responseGLM[((i-1)*D+1):(i*D)] <- rep (NA, D)
    responseGLM[(length(responseGLM)-pastWeeksNotIncluded*D):length(responseGLM)] <- NA
    
    dataGLM <- data.frame(response=responseGLM,wtime=wtimeGLM,population=popGLM,
                          seasgroups=as.factor(seasgroupsGLM),vectorOfDates=as.Date(vectorOfDatesGLM,origin="1970-01-01"),delay=delaysGLM)
    
  }
  
  
  
  
  
  return(as.data.frame(dataGLM))
  
}

################################################################################
# END OF DATA GLM FUNCTION
################################################################################

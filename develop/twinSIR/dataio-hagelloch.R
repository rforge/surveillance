######################################################################
# Prepare Hagelloch data for conversion to epidata object.  This file
# contains procedures for converting the data.frame sent by Pete Neal
# to an epidata object using the description given in Neal & Robert
# (2004).
#
# Author: Michael Höhle
# Date:   25 Mar 2011 
######################################################################

#Load the 'cool' packages
library("surveillance")
library("animation")

######################################################################
# Convert notation MM.DD to R date format. Note 00.00 corresponds
# to NA
#
# Parameters:
#  moday - a vector of two digit numerics in the format MM.DD.
# Returns:
#  a vector of class Date
######################################################################

moday2Date <- function(moday) {
  moday.str <- sprintf("%.2f",moday)
  moday <- matrix(as.numeric(unlist(strsplit(moday.str,split=".",fixed=TRUE))),ncol=2,byrow=TRUE)
  #Set 00.00 to NA's
  isNA <- moday[,1] == 0 & moday[,2] == 0
  #Convert only non NA's
  date <- as.Date(ifelse(!isNA, paste(ifelse(moday[,1]<10,1862,1861),moday[,1],moday[,2],sep="-"), NA))
  return(date)
}

######################################################################
# This is the conversion function
######################################################################

loadHagelloch <- function() {
  #Read data
  hagelloch <- read.table("hagelloch.txt",skip=43,header=TRUE)

  #time of prodomes (in Neal & Roberts its called time of symptoms)
  hagelloch$tS <-   hagelloch$PRO <- moday2Date(hagelloch$PRO)
  #time of rash
  hagelloch$tQ <-   hagelloch$ERU <- moday2Date(hagelloch$ERU)
  #time of death
  hagelloch$tD <- moday2Date(hagelloch$DEAD) 


  #Convert factors
  hagelloch$SEX <- factor(sapply(hagelloch$SEX+1, function(sex) switch(sex,"unknown","male","female")),levels=c("unknown","male","female"))
  hagelloch$CL <- factor(sapply(hagelloch$CL+1, function(cl) switch(cl,"preschool","1st class","2nd class")),levels=c("preschool","1st class","2nd class"))
  hagelloch$C <- factor(sapply(hagelloch$C+1, function(complication) switch(complication,"no complicatons","bronchopneumonia","severe bronchitis","lobar pneumonia","pseudocroup","cerebral edema")),levels=c("no complicatons","bronchopneumonia","severe bronchitis","lobar pneumonia","pseudocroup","cerebral edema"))

  #Convert location to meters
  hagelloch$HNX <-   hagelloch$HNX * 2.5
  hagelloch$HNY <-   hagelloch$HNY * 2.5
  
  ######################################################################
  #Problem: the dates have ties, because they are interval censored
  #         (how often did Pfeilsticker visit the village??)
  #         We will assume interval censoring within the day and the
  #         the day before (daily visits)
  ######################################################################

  #Break ties by subtracting random time
  set.seed(123)
  hagelloch$tS <- hagelloch$tS - runif(nrow(hagelloch))
  t0 <- min(hagelloch$tS)
  hagelloch$tS <- hagelloch$tS - t0

  for (state in c("tD","tQ")) {
    hagelloch[,state] <- hagelloch[,state] - runif(nrow(hagelloch)) - t0
  }

  ######################################################################
  # Generate other variables
  ######################################################################

  #Set tR using fixed d0 parameter
  d0 <- 3
  d1 <- 1

  #Generate unknown times as described on p.251 of the paper
  hagelloch$tR <- with(hagelloch, pmin(tD, tQ + d0,na.rm=TRUE))
  hagelloch$tI <- with(hagelloch, tS - d1)

  #Check that no more than 1 event per time
  if (sum(table(hagelloch$tS) > 1)) { stop("Ties in event times!") }

  #Done, return the data frame
  return(hagelloch)
}

######################################################################
# Convert to epidata using loop over events.
#
# Parameters:
#  pop.df - data.frame containing the event times of each individual
#           in the population. Needs to contain columns tS, tI, tR
#           and location as column names "x" and "y"
#  extraVarNames - vector of string containing additional columns of
#                  pop.df to include
#  startTime - time before first event where the analysis starts
#  
#  Returns:
#  epidata compatible framework
######################################################################

"%without%" <- function(x,y) x[!x %in% y] #--  x without y

df2history <- function(pop.df, xycoords.names=c("x","y"),extraVarNames=NULL) {
  ######################################################################
  #Helper function to determine who is infectious at a specific time point
  #I.e. determine set I(t) (i.e. all i, where t > t_i^{E->I} & t < t_i^{I->R})
  ######################################################################
  Infec <- function(t){
    (t > pop.df$tS) & (t <= pop.df$tR)
  }
  #Number of individuals in the population
  n <- nrow(pop.df)
  #Number of infections in the population
  nInfections <- sum(!is.na(pop.df$tS))
  #Ids of individuals
  id <- 1:nrow(pop.df)
  #Who are the first cases (here: only one)
  indexCase <- which.min(pop.df[!is.na(pop.df$tI),"tI"])

  #Time of S->E change. Condition on the first one already being infectious
  infectionTimes <- pop.df[!is.na(pop.df$tI),"tI"]
  #Time of E->I change. Again, conditioned on index cases
  infectiousTimes <- pop.df[!is.na(pop.df$tS),"tS"]
  #Time of removal
  removalTimes <- pop.df[!is.na(pop.df$tR),"tR"]
  #List of all times where something happened. Now sorted
  eventTimes <- sort(unique(c(infectionTimes[1:n %without% indexCase], infectiousTimes[1:n %without% indexCase], removalTimes)))
  #How many events in total (3*number of infected)
  nEvents <- length(eventTimes)
  
  #Distance matrix, ensure distance to itself is maximal
  Dkm <- as.matrix(dist(pop.df[,xycoords.names])/1000,diag=TRUE)
  diag(Dkm) <- Inf
  #These are distance based kernels. Can be replaced by f argument in as.epidata
#  Dkm.local <- (Dkm < (62.5/1000)) * is.finite(Dkm) #use 62.5 meter as local distance, see p.255 of Neal & Roberts (2004)
#  Dkm.global <- (Dkm >= (62.5/1000)) * is.finite(Dkm) 
  Class1 <-  with(pop.df, (CL == "1st class") %o% (CL == "1st class"))
  Class2 <-  with(pop.df, (CL == "2nd class") %o% (CL == "2nd class"))

  #Risk indicator
  Y <- as.numeric((1:n) != indexCase)

  evHist <- NULL
  nextEvent <- data.frame(id=id, x.loc=pop.df[,xycoords.names[1]]/1000, y.loc=pop.df[,xycoords.names[2]]/1000,
                          atRiskY=Y,
# Additional covariates for endemic or epidemic part. Note that extraVarNames
# are appended at the end
# hoehle @ 12 Aug 2014: local and global not needed anymore, because they
# are now generated directly from the as.epidata f function argument.
#                          local=rep(0,n),global=rep(0,n),
                          c1=rep(0,n), c2=rep(0,n),
# Time book-keeping
                          start=rep(0,n), stop=rep(0,n), event=rep(0,n),Revent=rep(0,n))

  #Add additional variable names
  if (!is.null(extraVarNames)) {
    nextEvent <- cbind(nextEvent,pop.df[,extraVarNames])
  }

  

  #Loop over all events
  for (i in seq_len(nEvents)) {
    t <- eventTimes[i]
    cat("\nLooking at event",i,"/",nEvents, "@ t=",t)
    
    
    #Update event bookeeping 
    nextEvent$start <- nextEvent$stop
    nextEvent$stop <- t

    #Surv stuff & risk indicator. Now also for Revent column
    infid <- match(t, infectionTimes, nomatch=0)
    #cat("InfID =",infid,"\n")
    recid <- match(t, removalTimes,   nomatch=0)
    nextEvent$event <- as.numeric(1:n == infid)
    nextEvent$Revent <- as.numeric(1:n == recid)
    
    nextEvent$atRiskY <- Y
    Y[infid] <- 0

    #Time varying covariates: They are assumed to change only at infection times.
    #None
    
    #Epidemic component: calculate infectious and weight by dist or class
    infective <- Infec(t)
    #School class indicator
    nextEvent$c1 <- as.numeric(rowSums(Class1[,infective,drop=FALSE]))
    nextEvent$c2 <- as.numeric(rowSums(Class2[,infective,drop=FALSE]))
    
    #Add last event to history
    evHist <- rbind(evHist, nextEvent)
  }
  #Add additional columns
  evHist$weights <- 1
  #Possibly retransform - not necessary here

  cat("\nFinished conversion!\n")
  return(evHist)
}

######################################################################
# Function to read hagelloch data and convert them to become
# an epidata object for use with the surveillance package
######################################################################

hagelloch2epidata <- function(saveInSurveillance=FALSE) {
  #Load data and produce R friendly data.frame
  hagelloch.df <- loadHagelloch()

  #Extra covariable names
  extraVarNames <- c("PN","NAME","FN","HN","AGE","SEX","PRO","ERU","CL","DEAD","IFTO","SI","C","PR","CA","NI","GE","TD","TM")
  
  #Convert data frame to a data.frame containing the history of the
  #inectious process. 
  hagelloch.evHist <- df2history(hagelloch.df, extraVarNames=extraVarNames,xycoords.names=c("HNX","HNY"))

  #Create epidata object. 
  hagelloch <- as.epidata(hagelloch.evHist,
                          id.col = "id", start.col = "start",
                          stop.col = "stop", atRiskY.col = "atRiskY",
                          event.col = "event", Revent.col = "Revent",
                          coords.cols = c("x.loc","y.loc"),
                          f = list(household = function(u) is.finite(u) & (u==0),
                            local = function(u) is.finite(u) & (u<0.0625),
                            global = function(u) is.finite(u) & (u>=0.0625),
                            nothousehold = function(u) is.finite(u) & (u>0)))


  #Save result as part of the surveillance package
  if (saveInSurveillance) {
    save(file="~/Surveillance/surveillance/pkg/data/hagelloch.RData",list=c("hagelloch","hagelloch.df"))
  }

  cat("Processes and saved hagelloch.RData\n")
  
  invisible(hagelloch)
}

doIt <- function() {
  source("dataio-hagelloch.R")
  #Make and write the data
  if (FALSE) {
      hagelloch <- hagelloch2epidata(saveInSurveillance=TRUE) #saved in pkg/data
  } else {
      hagelloch <- hagelloch2epidata(saveInSurveillance=FALSE)
  }
  
  #Load
  load(file="~/Surveillance/surveillance/pkg/data/hagelloch.RData")

  descriptive()
}


descriptive <- function() {
  library("surveillance")

  #Show case locations as in Neal & Roberts (different scaling)
  plot(hagelloch.df$HNX,hagelloch.df$HNY,xlab="x [m]",ylab="x [m]",pch=15)
  
  ######################################################################
  # Draw epicurve
  ######################################################################
  #Manual
  hist(as.numeric(hagelloch.df$tS),xlab="Time (days)",ylab="Cases",main="")

  
  #Epicurve package - does not work (?)
#library("epitools")
  source("helper.R")

  epicurve <- function(strata=NULL,col=c("darkgray","lightblue","indianred2"),leg.title=NULL) {
#  debug("epicurve.dates")
#  ec <- epicurve.dates(as.Date(hagelloch.df$PRO),before=0,after=0)
#    col <- c("darkgray","lightblue","indianred2")
#  col <- brewer.pal(length(levels(hagelloch.df$SEX)),"Spectral")
#ec <- epicurve.dates(as.Date(hagelloch.df$PRO),before=0,after=0,strata=hagelloch.df$SEX,col=col,tick.offset=0.0,las=2,cex.names=0.5)
#   ec <- epicurve.dates(as.Date(hagelloch.df$PRO),before=0,after=0,strata=hagelloch.df$SEX,col=col,tick.offset=0.0,segments=TRUE)
  #  ec <- epicurve.dates(as.Date(hagelloch.df$PRO),before=0,after=0,strata=hagelloch.df$SEX,col=col,tick.offset=0.0,names.arg="")#,tick=FALSE)
    #Draw the epicurve
    ec <- epicurve.dates(as.Date(hagelloch.df$PRO),before=0,after=0,strata=strata,col=col,tick.offset=0.0,axisnames=FALSE,ylab="No. prodomes starting")#,tick=FALSE)

    legend(x="topright",levels(strata),fill=col,title=leg.title)
    is.startOfMonth <- as.numeric(format(ec$cdates,"%d")) == 1
    is.15th <- as.numeric(format(ec$cdates,"%d")) == 15
    axis(1,at=ec$xvals[is.15th],labels=FALSE,tcl=-0.7)
    axis(1,at=ec$xvals[is.startOfMonth],labels=ec$cdates[is.startOfMonth],tcl=-0.9,lwd.ticks=1.5)
    axis(2)
  }

  epicurve(strata=hagelloch.df$CL,col=brewer.pal(length(levels(hagelloch.df$CL)),"Set2"))
  epicurve(strata=hagelloch.df$SEX)
  
#  debug("epicurve.weeks")
#  ec2 <- epicurve.weeks(hagelloch.df$PRO)#,before=0,after=0)
  
#  t0 <- min(hagelloch.df$PRO)
#  epicurve.dates(as.Date(hagelloch.df$tS + t0))

  #Draw extra lines on first of month
       
#  cat("Done with description\n")


  #hist(hagelloch$AGE)

  #Start of the outbreak (first symptoms)
  as.numeric(min(hagelloch$tS))
  #First S->I transmission
  as.numeric(min(hagelloch$tI))


  #Summary
  summary(hagelloch.epidata)

  stateplot(hagelloch.epidata, "187")  # see 'stateplot'

  plot(hagelloch.epidata)
  animate(hagelloch.epidata)
}

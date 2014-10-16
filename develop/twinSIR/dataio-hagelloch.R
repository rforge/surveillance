######################################################################
# Prepare Hagelloch data for conversion to epidata object.  This file
# contains procedures for converting the data.frame sent by Pete Neal
# to an epidata object using the description given in Neal & Roberts
# (2004). Latent periods (of usually 1 week) are not considered.
#
# 25 Mar 2011 (MH): initial version
# 16 Oct 2014 (SM): corrections
# - format DEAD as Date and set NAs in SEX, IFTO, TD and TM
# - use new converter function as.epidata.data.frame():
#   + in Michael's converter df2epidata(), Infec(), c1, c2 are wrong
#   + do not create redundant stop times at tS = tPRO
# - new t0 = 1861-10-30 00:00:00 (!), previously t0 was at min(tPRO)
# - new names for tS, tD, tQ in hagelloch.df: tPRO, tDEAD, tERU
# - coordinate names in hagelloch.df as in hagelloch (x.loc and y.loc)
# - do not include all variables in epidata to save space
######################################################################

#Load the 'cool' packages
library("surveillance")
library("animation")

######################################################################
# Convert notation MM.DD to R Date format. Note 0.00 corresponds to NA
#
# Parameters:
#  moday - a vector of two digit numerics in the format MM.DD.
# Returns:
#  a vector of class Date
######################################################################

moday2Date <- function (moday)
{
  moday.str <- sprintf("%.2f", moday)
  moday.mat <- matrix(as.numeric(unlist(
      strsplit(moday.str, split=".", fixed=TRUE)
  )), ncol=2, byrow=TRUE, dimnames = list(NULL, c("month", "day")))
  is.na(moday.mat) <- moday.mat == 0
  ymd <- cbind(year = ifelse(moday.mat[,"month"] < 10, 1862, 1861), moday.mat)
  as.Date(ifelse(complete.cases(ymd),
                 paste(ymd[,"year"], ymd[,"month"], ymd[,"day"], sep="-"),
                 NA))
}

######################################################################
# Import the text file and format columns -> hagelloch.df
######################################################################

importHagelloch <- function ()
{
  ## Import text file
  hagelloch <- read.table("hagelloch.txt", header=TRUE, quote="", dec=".",
                          blank.lines.skip=TRUE, comment.char="#",
                          stringsAsFactor=TRUE)

  ## Format
  hagelloch <- within(hagelloch, {
      ## factors
      SEX <- factor(SEX, levels=1:2, labels=c("male","female")) # 0=unknown=NA
      CL <- factor(CL, levels=0:2, labels=c("preschool","1st class","2nd class"))
      C <- factor(C, levels=0:5, labels=c("no complicatons","bronchopneumonia","severe bronchitis","lobar pneumonia","pseudocroup","cerebral edema"))

      ## convert "month.day" columns to the Date class
      PRO <- moday2Date(PRO)   # date of prodromes (first symptoms)
      ERU <- moday2Date(ERU)   # date of eruption (rash)
      DEAD <- moday2Date(DEAD) # date of death (with missings)
      
      ## coordinates to meter units
      HNX <- HNX * 2.5
      HNY <- HNY * 2.5

      ## missings
      is.na(IFTO) <- IFTO == 0
      is.na(TD) <- TD == 0
      is.na(TM) <- TM == 0
  })

  ## rename coordinate columns
  names(hagelloch)[names(hagelloch) == "HNX"] <- "x.loc"
  names(hagelloch)[names(hagelloch) == "HNY"] <- "y.loc"

  return(hagelloch)
}

  ######################################################################
  #Problem: the dates have ties, because they are interval censored
  #         (how often did Pfeilsticker visit the village??)
  #         We will assume interval censoring within the day and the
  #         the day before (daily visits)
  ######################################################################

untieHagelloch <- function (hagelloch)
{
  t0 <- as.Date("1861-10-30")
  
  ## Break ties by subtracting random time and make relative to t0
  set.seed(123)
  for (state in c("PRO", "DEAD", "ERU")) {
    hagelloch[[paste0("t",state)]] <-
        as.numeric(hagelloch[[state]] - t0) + 1 - runif(nrow(hagelloch))
  }
  ## e.g., tPRO = 0.5 means first symptoms on 1861-10-30 at noon
  
  ## Assume infectiousness until three days after appearance of rash (d0),
  ## and infection one day before the first symptoms (d1)
  ## as in Neal and Roberts (2004, Section 3.1).
  ## No latent periods, i.e., day of infection is start of infectiousness.
  d0 <- 3
  d1 <- 1

  ## Generate unknown times as described on p. 251 of the paper
  hagelloch$tR <- with(hagelloch, pmin(tDEAD, tERU + d0, na.rm=TRUE))
  hagelloch$tI <- hagelloch$tPRO - d1

  ## Done
  return(hagelloch)
}


######################################################################
# !!! OLD CONVERTER BY Michael, WE NOW USE as.epidata.data.frame() !!!
# CAVE: tS (=tPRO) is NOT the E->I change => Infec(), c1, c2 are WRONG
#       tI (=tS-1) is the E->I change.
#
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
#  Dkm <- as.matrix(dist(pop.df[,xycoords.names])/1000,diag=TRUE)
#  diag(Dkm) <- Inf
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
  #evHist$weights <- 1
  #Possibly retransform - not necessary here

  cat("\nFinished conversion!\n")
  return(evHist)
}

######################################################################
# OBSOLETE function to convert the basic hagelloch data frame
# to "epidata" using WRONG df2epidata()
######################################################################

hagelloch2epidata_old <- function(hagelloch.df, extraVarNames = NULL)
{
  # define tS (=tPRO) as expected by old df2history()
  hagelloch.df$tS <- hagelloch.df$tPRO
  
  # revert to old t0
  ## hagelloch.df$tS <- hagelloch.df$tS - min(hagelloch.df$tPRO)
  ## hagelloch.df$tI <- hagelloch.df$tI - min(hagelloch.df$tPRO)
  ## hagelloch.df$tR <- hagelloch.df$tR - min(hagelloch.df$tPRO)

  #Convert data frame to a data.frame containing the history of the
  #infectious process. CAVE: df2history creates redundant stop times at "tS".
  hagelloch.evHist <- df2history(hagelloch.df, extraVarNames=extraVarNames,
                                 xycoords.names=c("x.loc","y.loc"))

  # Create epidata object
  hagelloch <- as.epidata(hagelloch.evHist,
                          id.col = "id", start.col = "start",
                          stop.col = "stop", atRiskY.col = "atRiskY",
                          event.col = "event", Revent.col = "Revent",
                          coords.cols = c("x.loc","y.loc"),
                          f = list(household = function(u) is.finite(u) & (u==0),
                            local = function(u) is.finite(u) & (u<0.0625),
                            global = function(u) is.finite(u) & (u>=0.0625),
                            nothousehold = function(u) is.finite(u) & (u>0)))
  
  return(hagelloch)
}


######################################################################
# use NEW converter as.epidata.data.frame()
######################################################################

hagelloch2epidata <- function (hagelloch.df, extraVarNames = NULL)
{
  # use new data.frame converter and meter-based coordinates,
  # we no longer have redundant stop times at "tS"
  hagelloch <- as.epidata(
      hagelloch.df, t0 = 0, tI.col = "tI", tR.col = "tR",
      id.col = "PN", coords.cols = c("x.loc", "y.loc"),
      f = list(
          household = function(u) is.finite(u) & (u==0),
          local = function(u) is.finite(u) & (u<62.5),
          global = function(u) is.finite(u) & (u>=62.5),
          nothousehold = function(u) is.finite(u) & (u>0)
      ),
      keep.cols = extraVarNames)

  #TODO: School class indicators


  return(hagelloch)
}



doIt <- function()
{
  source("dataio-hagelloch.R")

  ## Import data and produce R friendly data.frame without ties
  hagelloch.df <- untieHagelloch(importHagelloch())

  # variables to keep in the "epidata"
  extraVarNames <- c("SEX", "AGE", "CL")

  #Make the data
  hagelloch <- hagelloch2epidata(hagelloch.df, extraVarName=extraVarNames)

  if (FALSE) { # use old implementation for comparison
      hagelloch_old <- hagelloch2epidata_old(hagelloch.df, extraVarNames = extraVarNames)
      ## drop redundant blocks with stop at tPRO
      hagelloch_old <- subset(hagelloch_old, stop %in% stop[event|Revent])
      nam <- intersect(names(hagelloch), names(hagelloch_old))
      all.equal(hagelloch_old[nam], as.data.frame(hagelloch)[nam])
  }

  #Save result as part of the surveillance package
  if (FALSE) {
    save(hagelloch, hagelloch.df, file = "../../pkg/data/hagelloch.RData")
  }

  if (FALSE) {
    descriptive()
  }
}


descriptive <- function() {
  ## library("surveillance")
  ## data("hagelloch")

  #Show case locations as in Neal & Roberts (different scaling)
  plot(hagelloch.df$x.loc,hagelloch.df$y.loc,xlab="x [m]",ylab="x [m]",pch=15)
  
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

  #Start day of the outbreak (first symptoms)
  min(hagelloch.df$PRO)
  min(hagelloch.df$tPRO)
  #First S->I transmission
  min(hagelloch.df$tI)


  #Summary
  summary(hagelloch)

  stateplot(hagelloch, "187")  # see 'stateplot'

  plot(hagelloch)
  animate(hagelloch)
}

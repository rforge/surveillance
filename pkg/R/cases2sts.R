######################################################################
# Takes a data frame with dates of individual
# cases and create an aggregated sts time series object for these
# data with aggregation occuring at the desired scale.
# Note: This function is experimental
#
# Parameters:
#  cases - a data frame containing individual case information per line
#  dateCol - a character string denoting the column name in case containing
#            the relevant date variable to aggregate
#  aggregate.by - aggregation block length given as a string compatible with
#       seq.Date -- see \link{seq.date} for further details.
#
# Author: Michael Hoehle
# Date LaMo: 24 Jan 2012
######################################################################

cases2sts <- function(cases,dateCol,aggregate.by="1 week",dRange=NULL) {
 #by <- match.arg(by,c("1 day","1 week","1 month","1 year")
  dRange <- range(cases[,dateCol],na.rm=TRUE)

  #Make sure that if weeks we span the entire data set.
  if ((aggregate.by=="1 week" | aggregate.by == "7 day") & is.null(dRange)) {
    #Adjust first date to a monday and the last to be a sunday
    weekDay <- as.numeric(format(dRange, "%w"))
    dRange[1] <- dRange[1] - ifelse( weekDay[1] == 0, 6, (weekDay[1]-1))
    dRange[2] <- dRange[2] + 7
  }
  
  dates <- seq(min(dRange), max(dRange), by=aggregate.by)

  #Make a table containing the specific number of cases. Note that this
  #needs to occur using a cut statement
  lvl <- cut(cases[,dateCol], breaks=dates,right=FALSE)

  observed <- table(lvl)
  epoch <- as.Date(names(observed))

  #Translate "by" to freq string
  freq <- switch(aggregate.by,"1 day"=365,"7 day"=52,"1 week"=52,"1 month"=12)
  
  observed <- matrix(observed,ncol=1)
  sts <- new("sts",epoch=as.numeric(epoch),observed=observed, alarm=0*observed, epochAsDate=TRUE,freq=freq)

  #Return
  return(sts)
}

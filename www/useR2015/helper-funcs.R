###Function to plot one
plotOne <- function(sts,dNow,dUp2,ylim,xlim, survControl, alarmVec=NULL, stsTruth=NULL) {
  sts2 <- sts
  thePast <- (epoch(sts2) < dNow)
  sts2@observed[!thePast,] <- 0

  ##Visualize using ordinary week and year not ISO8601 - coz then months
  ##are not consistent with years.
  args <- list(xlab="Time (weeks)",
               ylab="Homicides by headshots & decapitation",
               xaxis.tickFreq=list("%W"=atChange,"%m"=atChange,"%Y"=atChange),
               xaxis.labelFreq=list("%Y"=atMedian),xaxis.labelFormat="%Y",
               legend.opts=NULL,col=c("lightgray","black",NA),
               alarm.symbol=list(pch=24, col="indianred1", cex=1, lwd=1.5),
               ylim=ylim)

  set.seed(123)
  rangeTest <- which( (epoch(sts) >= dNow) & (epoch(sts) <= dUp2))

  ##Perform surveillance
  s.far <- bodaDelay(sts,modifyList(survControl,list(range=rangeTest)))#,inferenceMethod="INLA")))
  if (!is.null(alarmVec)) {
    alarms(s.far) <- matrix(alarmVec,ncol=1)
  }

  #Determine dates
  first <- which(epoch(sts2) == dNow) #+ 1
  up2   <- which(epoch(sts2) == dUp2)

  ##For visualization
  sts3 <- sts2
###hoehle - this doesn't work when there are delays  sts3@observed[first:up2,] <- sts@observed[first:up2,]
  sts3@observed[1:up2,] <- sts@observed[1:up2,]
  sts3@alarm[first:up2,] <- s.far@alarm[seq_len(up2-first+1),]
  sts3@upperbound[first:up2,] <- s.far@upperbound[seq_len(up2-first+1),]

  args <- modifyList(args, list(xlim=xlim,
                     xaxis.tickFreq=list("%W"=atChange,"%m"=atChange,"%Y"=atChange),
                     xaxis.labelFreq=list("%Y%m"=atChange),xaxis.labelFormat="%b-%Y"))

  ##Start by truth (if it exists)
  if (!is.null(stsTruth)) {
    stsTruth@observed[seq_len(nrow(stsTruth)) > up2] <- 0
    do.call("plot",modifyList(args,list(x=stsTruth,col=c(NA,"black",NA))))
###        do.call("plot",modifyList(args,list(x=stsTruth,col=c("snow","black",NA))))
  }
  ##Add current time series
  do.call("plot",modifyList(args,list(x=sts3,add=!is.null(stsTruth),col=c("lightgray","black",NA))))


  sts4 <- sts3
  observed(sts4)[thePast,] <- 0

  ##Add surveil'ed time points
  do.call("plot",modifyList(args,list(x=sts4,add=TRUE,col=c("darkgray","black",NA))))

  sts5 <- sts4
  observed(sts5)[!alarms(sts5),] <- 0
  upperbound(sts5)[up2+1,] <- upperbound(sts5)[up2,]
  do.call("plot",modifyList(args,list(x=sts5,add=TRUE,col=c("indianred1","black","darkred"),lwd=c(1,1,3),lty=c(1,1,1))))


  invisible(min(epoch(s.far)[alarms(s.far)==1]))
}


###Make an sts object with what has arrived by a particular date
stsByDate <- function(date) {
  ###Matrix of date an observation arrives
  tab <- outer(as.numeric(epoch(stsw)), (seq_len(length(pmfDelay))-1)*7, FUN="+")

  rT[tab > as.numeric(date)] <- NA
  ##Resulting surveillance time series object
  stswDelay <- stsw
  observed(stswDelay) <- as.matrix(rowSums(rT,na.rm=TRUE),ncol=1)
  ##Done
  return(stswDelay)
}

addNow2Plot <- function(dateNow, withDate=TRUE) {
  idxNow <- which(epoch(stsw) == dateNow)

  labels <- if (withDate) { formatDate(dateNow,"%d %b") } else { NA }

  axis(1,at=idxNow,labels=labels,lwd=0,line=-2,cex.axis=0.7,col="green")
  axis(1,at=idxNow,labels=NA,col="green",lwd=2,cex=2)
  points(idxNow,0,pch=13,col="green")
}

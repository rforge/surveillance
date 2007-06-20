library(methods)
library(maptools)
library(surveillance)
library(vcd)
library(sp)

setClass( "sts", representation(week = "numeric",
                                freq = "numeric",
                                observed = "matrix",
                                state = "matrix",
                                alarm = "matrix",
                                upperbound  = "matrix",
                                neighbourhood= "matrix",
                                populationFrac= "matrix",
                                map = "SpatialPolygonsDataFrame",
                                control = "list"))


init.sts <- function(.Object, week, freq=52, observed, state, map=NULL, neighbourhood=NULL, populationFrac=NULL,alarm=NULL,upperbound=NULL) {
  #Name handling
  namesObs <-colnames(observed)
  namesState <- colnames(observed)

  #check number of columns of observed and state
  nAreas <- ncol(observed)
  nObs <- nrow(observed)
  if(ncol(observed) != ncol(state)){
    #if there is only one state-vector for more than one area, repeat it
    if(ncol(state)==1)
      state <- ts(matrix(rep(state,nAreas),ncol=nAreas,byrow=FALSE),freq=frequency(observed))
    else{ 
      cat('wrong dimensions of observed and state \n')
      return(NULL)
    }
  }
  
  #check neighbourhood matrix
  if(!is.null(neighbourhood) & (any(dim(neighbourhood) != nAreas))) {
    cat('wrong dimensions of neighbourhood matrix \n')
    return(NULL)
  }

  #popFrac
  if (is.null(populationFrac)) {
    populationFrac <- matrix(1/nAreas,nrow=nObs,ncol=nAreas)
  }
  if (nAreas ==1){
    populationFrac <- matrix(1,nrow=nObs, ncol=1)
  }
  
  #labels for observed and state
  if(is.null(namesObs)){
    namesObs <- paste("observed", 1:nAreas, sep="")       
    namesState <- paste("state", 1:nAreas, sep="")  
  }
 
  dimnames(observed) <- list(NULL,namesObs)
  dimnames(state) <- list(NULL,namesState)

  if (is.null(alarm)) 
    alarm      <- matrix(NA,nrow=dim(observed)[1],ncol=dim(observed)[2])
  if (is.null(upperbound))
  upperbound <- matrix(NA,nrow=dim(observed)[1],ncol=dim(observed)[2])

  ##Assign everything
  .Object@week <- week
  .Object@freq <- freq
  .Object@observed <- observed
  .Object@state <- state
  .Object@map <- map
  .Object@neighbourhood <- neighbourhood
  .Object@populationFrac <- populationFrac
  .Object@alarm <- alarm
  .Object@upperbound <- upperbound

  return(.Object)
}

#Two types of initialize methods
setMethod("initialize", "sts", init.sts)
setMethod("initialize", "sts", function(.Object, disProg, freq=52, map=NULL) {
  init.sts(.Object, week=disProg$week, freq=freq, observed=disProg$observed, state = disProg$state, map=map, neighbourhood=disProg$neighbourhood, populationFrac=disProg$populationFrac,alarm=disProg$alarm,upperbound=disProg$upperbound)
#  .Object <- initialize(.Object, week=disProg$week, freq=freq, observed=disProg$observed, state = disProg$state, map=map, neighbourhood=disProg$neighbourhood, populationFrac=disProg$populationFrac,alarm=disProg$alarm,upperbound=disProg$upperbound)
})

  
##Generics
library(methods)
if(!isGeneric("plot")) setGeneric("plot", useAsDefault=plot)
if(!isGeneric("aggregate")) setGeneric("aggregate", useAsDefault=aggregate)

#Method to aggregate over all units.
setMethod("aggregate", signature(x="sts"), function(x,nfreq="all",...) {
  if (nfreq == "all") {
    howmany <- dim(x@observed)[1]
  } else {
    if (nfreq != x@freq) {
      howmany <- x@freq / nfreq
      if (howmany - ceiling(howmany) != 0) { stop("Error: nfreq has to be a multiple of x@freq.")}
    }
  }
  
  n <- dim(x@observed)[1]
  m <- ceiling(n/howmany)
  new <- rep(1:m,each=howmany)[1:n]
  x@freq <- nfreq
  x@week <- 1:m
 
  x@observed <- as.matrix(aggregate(x@observed,by=list(new),sum)[,-1])
  x@state <- as.matrix(aggregate(x@state,by=list(new),sum)[,-1])>0
  x@alarm <- as.matrix(aggregate(x@alarm,by=list(new),sum)[,-1])
  x@upperbound <- as.matrix(aggregate(x@upperbound,by=list(new),sum)[,-1])
  
  #Small helper function to aggregate one mts slot of an sts object
  #xaggregate <- function(name) {
  #  y <- eval(substitute(apply(x@n,MAR=1,sum),list(n=name)))
  #  return(matrix(y,ncol=1))
  #}
  
  return(x)
})
  
setMethod("plot", signature(x="sts", y="missing"), function(x, y, type,...) {
  #Parse the formula
  obsOk <- type[[2]] == "observed"
  map   <- (length(type[[3]])==3) && (type[[3]][[1]] == "|") && (type[[3]][[2]] == "1")
  time  <- pmatch("time",type[[3]]) > 0
  valid <- lapply(as.list(type[[3]]),function(i) is.na(pmatch(i,c("1","unit","|","time","*","+"))))
  valid <- all(!unlist(valid))
  justTime <- type[[3]] == "time"
  
  if (!obsOk | !valid) {
    stop("Error: Not a valid plot type.")
  }


  #space-time plots
  if (map) {
    plot.disProg.spacetime(x,type,...)
    return(invisible())
  }
  #time plots
  if (time) {
    #In case observed ~ time, the units are aggregated
    plot.disProg.time(if(justTime) aggregate(x,nfreq="all") else x,type,...)
    return(invisible())
  }
})

add.legend <- function(legend,maplim,theColors) {
  #Preproc
  dy <- diff(maplim$y) * legend$dy
  dx <- diff(maplim$x) * legend$dx
    
  #Add legend -- i.e. a slider
  xlu <- xlo <- legend$x
  xru <- xro <- xlu + dx 
  yru <- ylu <- legend$y
  yro <- ylo <- yru + dy 

  
  step <- (xru - xlu)/length(theColors$col)
  for (i in 0:(length(theColors$col) - 1)) {
    polygon(c(xlo + step * i, xlo + step * (i + 1), 
              xlu + step * (i + 1), xlu + step * i), c(ylo, 
                                                       yro, yru, ylu), col = theColors$col[i + 1], 
            border = theColors$col[i + 1])
  }
  
  
  #Write info about min and max on the slider.
  black <- grey(0)
  lines(c(xlo, xro, xru, xlu, xlo), c(ylo, yro, yru, ylu, ylo), col =   black)

  #Transformation function for data values, either
  #exp or identical
  trans <- theColors$trans

  text(xlu, ylu, formatC(trans(theColors$min)), cex = 0.8, col = black,adj=c(0,1))
  text(xru, yru, formatC(trans(theColors$max)), cex = 0.8, col = black,adj=c(1,1))
}

gyr.colors <- function(x,ncolors=100,use.color=TRUE) {
  #Color scale
  if (use.color) {
    GYR <- c(rgb(1:(ncolors/2),(ncolors/2),0,maxColorValue=(ncolors/2)),
             rgb((ncolors/2),(ncolors/2):1,0,maxColorValue=(ncolors/2)))
  } else {
    GYR <- gray(((ncolors-1):0)/(ncolors-1))
  }
  return(list(col=GYR,min=0,max=max(x), trans=function(x) return(x)))
}


  

plot.disProg.time.one <- function(x, k=1, title = "", ylim=NULL, legend=TRUE, ...) {
  observed   <- x@observed[,k]
  state      <- x@state[,k]
  alarm      <- x@alarm[,k]
  upperbound <- x@upperbound[,k]
  hasAlarm   <- all(!is.na(alarm))
  # width of the column
  tab <- 0.5

  # left/right help for constructing the columns
  observedxl  <- (1:length(observed))-tab
  observedxr  <- (1:length(observed))+tab
  upperboundx <- (1:length(upperbound))-tab  
  # control where the highest value is
  #max <- max(observed)
  max <- max(c(observed,upperbound),na.rm=TRUE)
  
  
  #if ylim is not specified
  if(is.null(ylim)){
    ylim <- c(-1/20*max, max)
  }
  
  xstuff <- cbind(observedxl, observedxr, upperboundx)
  ystuff <-cbind(observed, observed, upperbound)
  
  
  matplot(xstuff,ystuff ,
          t="hhs", lty=c(1,1,1), col=c(1,1,4), ylim=ylim,
          xlab = "time", ylab = "No. of infected", ...)

  #Add bars and add points for the alarms and known outbreak times
  for(i in 1:length(observed)){
    matlines( c(i-tab, i+tab), c(observed[i],observed[i]) )
    if (!is.na(alarm[i]) && (alarm[i] == 1))
      matpoints( i, -1/40*max, pch=24, col=2)
    if(state[i] == 1)
      matpoints( i, ylim[1], pch=24, col=3)
  }
  
  if (is.null(title)) {
    if (!is.null(x@control)) {
      method <- x@control$name
      disease <- x@control$data
      if (disease != "") { disease <- paste("of ",disease," ",sep="") }
      title(paste("Analysis ", as.character(disease), "using ", as.character(method),sep=""))
    }
  } else {
    title(title)
  }

  #Aggregated time series..not used atm
  #if (!is.null(x@aggr)) {
  #  points(upperboundx+tab,x@aggr,col=1)
  #}
  
  cex <- par()$cex.axis  
  #should there be a legend?
  if(legend){
   # parameters for the legend placement to the right upper side
    xlegpos <- 3/4
    ylegpos <- 1

    # check where to place the legend. If the left upper side is free place it there
    if (max * 2/3 >= max(observed[1:floor(1/4 * length(observed))])){
      xlegpos <- 0
    }

    if (hasAlarm) {
      legend(xlegpos*length(observed)/sqrt(cex), ylegpos*max,
             legend=c("Infected", "Threshold", "Computed Alarm", "Defined Alarm"),
             lty=c(1,1,NA,NA), col=c(1,4,2,3), pch=c(NA,NA,24,24),cex=cex)
    } else {
      legend(xlegpos*length(observed), ylegpos*max,
             legend=c("Infected", "Defined Alarm"),
           lty=c(1,NA), col=c(1,3), pch=c(NA,24),cex=cex)
    }
  }
  invisible()
}

plot.disProg.time <- function(x, type,title = "",same.scale=TRUE, legend=TRUE, ...){
  #Plot as one if type = time + unit 
  as.one=all(!is.na(pmatch(c("time","unit"),type[[3]] ))) & is.na(pmatch("|",type[[3]]))
  #Extract observed
  observed <- x@observed
  nAreas <- ncol(observed)
  max <- max(observed)
  
  #multivariate time series
  if(nAreas > 1){
    #all areas in one plot 
    if(as.one){
      par(mfrow=c(1,1))

      ylim <- c(-1/20*max, max)
      matplot(observed,type="l",lty=1:nAreas,col=1:nAreas,ylim=c(0, 1.1*max),xlab="time",ylab="No. of Infected", ...)

      alarm <- apply(x@alarm,MARGIN=1,sum)>0
      state <- apply(x@state,MARGIN=1,sum)
      for (i in 1:dim(observed)[1]) {
        if (!is.na(alarm[i]) && (alarm[i] == 1))
          matpoints( i, -1/40*max, pch=24, col=2)
        if(state[i] == 1)
          matpoints( i, ylim[1], pch=24, col=3)
      }
    
      if(legend)                                                                             
        legend("topleft",legend=colnames(observed),col=1:nAreas,lty=1:nAreas,ncol=5, bty="n")
        
      title(title)
    } else {  #plot each area
      #set window size     
      par(mfrow=magic.dim(nAreas),mar=c(2,1,1,1))
      
      if(same.scale)
        ylim <- c(-1/20*max, max)
      else
        ylim <- NULL
      
      #plot areas
      k <- 1:nAreas
      sapply(k, function(k) {
        plot.disProg.time.one(x,k=k, title = "", ylim=ylim, legend=FALSE,... )   
         mtext(colnames(observed)[k],line=-1.3)     
         })
      #reset graphical params
      par(mfrow=c(1,1), mar=c(5, 4, 4, 2)+0.1)
    }
  } else {  #univariate time series
    plot.disProg.time.one(x=x, title = title, legend=legend, ...)
  }
  invisible()
}


plot.sts.spacetime <- function(x,type,legend=NULL,opts.col=NULL,labels=TRUE,wait=1e6,axes=TRUE,cex.lab=0.7,verbose=FALSE,...) {
  #Extract the mappoly
  if (is.null(x@map))
    stop("Error: The disProg Xect doesn't have a map entry.")
  map <- x@map
  maplim <- list(x=bbox(map)[1,],y=bbox(map)[2,])
  
  #Check for color options
  if (is.null(opts.col)) {
    opts.col <- list(ncolors=100,use.color=TRUE)
  }
  #Check for legend options
  if (is.null(legend)) {
    legend <- list(dx=0.4,dy=0.02,x=maplim$x[1],y=maplim$y[1],once=TRUE)
  }
  #Extract the data
  o <- x@observed
  alarm <- x@alarm
  
  #Formula is of type "observed ~ 1|unit" (i.e. no time)
  aggregate <- type[[3]][[3]] == "unit"
  if (aggregate) {
    o <- t(as.matrix(apply(o,MARGIN=2,sum)))
    alarm <- t(as.matrix(apply(alarm,MARGIN=2,sum)))>0
  }
  
  #Number of time points
  maxt <- dim(o)[1]
  
  #Make colors
  #gyr <- gyr.colors(o,ncolors=opts.col$ncolors,use.color=opts.col$use.color)
  #gyr <- hcl.colors(o,ncolors=opts.col$ncolors,use.color=opts.col$use.color)
  gyr <- hcl.colors(o,ncolors=length(o),use.color=TRUE)

  #Cut into specified number of colors
  o.cut <- matrix(as.numeric(cut(o,length(gyr$col))),nrow=nrow(o),ncol=ncol(o))
  o.col <- matrix(gyr$col[o.cut],ncol=ncol(o.cut))
  o.col[is.na(o.col)] <- gray(1)
  dimnames(o.col) <- dimnames(o)

  #Sort the o xected according to the names in the map xect
  region.id <- unlist(lapply(map@polygons,function(poly) poly@ID))
  o.col.id <- dimnames(o.col)[[2]]

  #Make the columns of o as in the map object
  o.col <- o.col[,pmatch(region.id,o.col.id),drop=FALSE]

  #Screen processing
  screen.matrix <- matrix(c(0,1,0,1,0,1,0.8,1),2,4,byrow=T)
  split.screen(screen.matrix)
  
  for (t in 1:maxt) {
    #Status information
    if (verbose) {
      cat(paste("Processing slice",t,"of",maxt,"\n"))
    }
    #Clean screen (title area)
    screen(2)
    par(bg=gray(1))
    erase.screen()
    par(bg="transparent")

    #Plot the map on screen 1
    screen(1)
    plot(map,col=o.col[t,],xlab="",ylab="",axes=axes,...)
    plot(map,dens=alarm*15,add=TRUE)
    

    if (labels)
      text(getSpPPolygonsLabptSlots(map), labels=as.character(region.id), cex.lab=cex.lab)
  
    if (!aggregate) { title(paste(t,"/",maxt,sep="")) }

    #In case a legend is requested
    if (!is.null(legend) && !(legend$once & t>1)  | (t==1)) {
      add.legend(legend,  maplim ,gyr)
    }
    #Is there a smarter way to specify a fixed waiting time?
    for(i in 1:wait) {}
  }
  close.screen(all.screens = TRUE)
}



# Implementation of the Bayes system.
# The system evaluates specified timepoints and gives alarm if it recognizes
# an outbreak for this timepoint.
#
# Features:
# Choice between different Bayes sub-systems (difference in reference values).

algo.bayesLatestTimepoint <- function(sts, timePoint = NULL, control = list(b = 0, w = 6, actY = TRUE, alpha=0.05)){

  observed <- sts@observed
  #Frequency
  freq <- sts@freq

  # If there is no value in timePoint, then take the last value in observed
  if(is.null(timePoint)){
        timePoint = dim(observed)[1]
  }

  #If no level specified.
  
  # check if the vector observed includes all necessary data.
  if((timePoint-(control$b*freq)-control$w) < 1){
        stop("The vector of observed is too short!")
  }

  # construct the reference values
  basevec <- NULL
  # if actY == TRUE use also the values of the year of timepoint
  if(control$actY){
        basevec <- observed[(timePoint - control$w):(timePoint - 1),]
  }
  # check if you need more referencevalues of the past
  if(control$b >= 1){
    for(i in 1:control$b){
        basevec <- rbind(basevec, observed[(timePoint-(i*freq)-control$w):(timePoint-(i*freq)+control$w),])
    }
  }

  # get the parameter for the negative binomial distribution
  sumBasevec <- apply(basevec,MARGIN=2,sum)
  lengthBasevec <- dim(basevec)[1]

  # compute the upper limit of the 95% CI.
  upCi <- qnbinom(1-control$alpha, sumBasevec + 1/2, (lengthBasevec)/(lengthBasevec + 1))

  # give alarm if the actual value is larger than the upper limit.
  alarm <- observed[timePoint,] >= upCi
  result <- list(alarm=alarm, upperbound=upCi)

  return(result)
}

# 'algo.bayes' calls 'algo.bayesLatestTimepoint' for data points given by range.

algo.bayes <- function(sts, control = list(range = range, b = 0, w = 6, actY = TRUE,alpha=0.05)){
  # Set the default values if not yet set
  if(is.null(control$b)){
    # value from bayes 1
    control$b <- 0
  }
  if(is.null(control$w)){
    # value from bayes 1
    control$w <- 6
  }
  if(is.null(control$actY)){
    # value from bayes 1
    control$actY <- TRUE
  }
  
  #no of areas
  nAreas <- ncol(sts@observed)

  # initialize the necessary vectors
  n <-  length(control$range)
  alarm <- matrix(data = 0, nrow = n, ncol = nAreas)
  upperbound <- matrix(data = 0, nrow = n, ncol = nAreas)

  count <- 1
  for(i in control$range){
    # call algo.bayesLatestTimepoint
    result <- algo.bayesLatestTimepoint(sts, i, control = control)
    # store the results in the right order
    alarm[count,] <- result$alarm
    upperbound[count,] <- result$upperbound
    count <- count + 1
  }
  #Add name and data name to control object.
  control$name <- paste("bayes(",control$w,",",control$w*control$actY,",",control$b,")",sep="")
  control$data <- paste(deparse(substitute(sts)))

  sts <- sts
  #Chop sts so only control$range values occurs
  sts@week <- sts@week[control$range]
  sts@observed <- sts@observed[control$range,]
  sts@state <- sts@state[control$range,]
  # return alarm and upperbound vectors
  sts@alarm <- alarm
  sts@upperbound <- upperbound
  sts@control <- control
  
  return(sts)
}

algo.bayes1 <- function(sts, control = list(range = range)){
  algo.bayes(sts, control = list(range = control$range, b = 0, w = 6, actY = TRUE,alpha=0.05))
}
algo.bayes2 <- function(sts, control = list(range = range)){
  algo.bayes(sts, control = list(range = control$range, b = 1, w = 6, actY = TRUE,alpha=0.05))
}
algo.bayes3 <- function(sts, control = list(range = range)){
  algo.bayes(sts, control = list(range = control$range, b = 2, w = 4, actY = FALSE,alpha=0.05))
}


hepa.load <- function() {
  #shape <- "berlin.shp"
  shape <- "z:/Map/Berlin/berlin.shp"
  hepMale <- read.table("hepAmale.txt",header=TRUE,skip=1)

  #Aggregate months
  year <- rep(2000+c(1,2,3,4,5,6),each=53)[1:dim(hepMale)[1]]
  month <- rep(rep(1:12,times=c(rep(4,11),5)),length.out=dim(hepMale)[1])
  mid <- month*1e4+year
  hepMale <- as.matrix(aggregate(hepMale,by=list(mid),sum)[,-1])
  #Re-arrange the week numbers
  hepMale[,1] <- 1:dim(hepMale)[1]

  #Create the new sts object
  hepa.berlin <- new("sts",week=hepMale[,1],freq=12,observed=hepMale[,-c(1,2,3)],
                     state=matrix(0,dim(hepMale)[1],dim(hepMale)[2]-3),
                     map=readShapePoly(shape,IDvar="SNAME"))
}

measels.load <- function() {
  data(measels.weser)
  return(new("sts",measels.weser$week,freq=52,measels.weser$observed,measels.weser$state,
             map=readShapePoly("z:/Map/WeserEms/weserems.shp",IDvar="KRS_SCHL")))
}

hcl.colors <- function(x,ncolors=100,use.color=TRUE) {
  if (use.color) {
    #The Zeil-ice colors 
    GYR <- rev(heat_hcl(ncolors, h=c(0,120), c=c(90,30), l=c(50,90), power=c(0.75, 1.2)))
  } else {
    #Sanity check
    GYR <- rev(heat_hcl(ncolors, h=c(0,120), c=0, l=c(50,90), power=c(0.75, 1.2)))
  }
  return(list(col=GYR,min=0,max=max(x), trans=function(x) return(x)))
}


compstat <- function() {
  source("class.R")
  data(ha)
  shapeFile <- paste(Sys.getenv("HOME"),"Map/Berlin/berlin.shp",sep="")
  ha <- new("sts",ha,map=readShapePoly(shapeFile,IDvar="SNAME"))
  ha4 <- aggregate(ha,nfreq=13)
  ha4.b62 <- algo.bayes(ha4,control=list(range=52:73,b=2,w=6,alpha=0.001))
  plot(ha4.b62,type=observed ~ time | unit)

  pdf("ha4b62.pdf",onefile = FALSE, width=9, horizontal = T)
  par(mar=c(0,0,0,0))
  plot(ha4.b62,type=observed ~ 1 | unit,axes=FALSE)
  dev.off()
}

foo <- function() {
  #source("gis.R")
  source("class.R")
  #source("gis.R")
  #sts <- hepa.load()
  #sts <- measels.load()
  
  plot(sts, type = observed ~ time)
  plot(sts, type = observed ~ time | unit)
  plot(sts, type = observed ~ 1 | unit)
  plot(sts, type = observed ~ 1 | unit * time)

  #First attempt to do multivariate surveillance
  noUnits <- 12
  y2001 <- 1:noUnits
  y2002 <- (1*noUnits+1):(2*noUnits)
  y2003 <- (2*noUnits+1):(3*noUnits)
  y2004 <- (3*noUnits+1):(4*noUnits)
  y2005 <- (4*noUnits+1):(5*noUnits)
  y2006 <- (5*noUnits+1):dim(sts@observed)[1]

  #Use the Bayes Algo
  ab <- algo.bayes(sts,control=list(range=c(y2005,y2006),b=2,w=1,alpha=0.001))
  
  plot(ab,type=observed ~ time | unit)
  plot(ab,type=observed ~ time + unit)
  plot(ab,type=observed ~ 1 |unit)
  plot(ab,type=observed ~ 1 |unit*time)
}

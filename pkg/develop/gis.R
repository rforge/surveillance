library(surveillance)

aggregateDisProg <- function(disProgObj){
  #aggregate observed counts
  observed <- apply(disProgObj$observed,MAR=1,sum)
  #aggregate states
  state <- apply(disProgObj$state,MAR=1,sum)
  state[state > 1] <- 1

  #aggregate alarm
  alarm <- apply(disProgObj$alarm,MAR=1,sum)>0
  #aggregate upperbound
  upperbound <- apply(disProgObj$upperbound,MAR=1,sum)>0
  
  #create univariate disProg object
  disProgObj <- create.disProg(week=disProgObj$week, observed=observed, state=state)
  disProgObj$alarm <- alarm
  disProgObj$upperbound <-upperbound
  
  return(disProgObj)
}

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


plot.disProg.spacetime <- function(obj,type,legend=NULL,opts.col=NULL,labels=TRUE,wait=1e6,axes=TRUE,cex.lab=0.7,verbose=FALSE,...) {
  #Extract the mappoly
  if (is.null(obj$map))
    stop("Error: The disProg Object doesn't have a map entry.")
  map <- obj$map
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
  x <- obj$observed
  alarm <- obj$alarm
  
  #Formula is of type "observed ~ 1|unit" (i.e. no time)
  aggregate <- type[[3]][[3]] == "unit"
  if (aggregate) {
    x <- t(as.matrix(apply(x,MARGIN=2,sum)))
    alarm <- t(as.matrix(apply(alarm,MARGIN=2,sum)))>0
  }
  
  #Number of time points
  maxt <- dim(x)[1]
  
  #Make colors
  gyr <- gyr.colors(x,ncolors=opts.col$ncolors,use.color=opts.col$use.color)

  #Cut into specified number of colors
  x.cut <- matrix(as.numeric(cut(x,length(gyr$col))),nrow=nrow(x),ncol=ncol(x))
  x.col <- matrix(gyr$col[x.cut],ncol=ncol(x.cut))
  x.col[is.na(x.col)] <- gray(1)
  dimnames(x.col) <- dimnames(x)

  #Sort the x objected according to the names in the map object
  #as.character(map[[obj$mapIDVar]])
  region.id <- unlist(lapply(map@polygons,function(poly) poly@ID))
  x.col.id <- dimnames(x.col)[[2]]

  #Make the columns of x as in the map object
  x.col <- x.col[,pmatch(region.id,x.col.id),drop=FALSE]

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

    screen(1)
    #plot(map,col=x.col[t,],xlab="",ylab="",axes=axes,...)
    plot(map,col=ifelse(!alarm[t,],x.col[t,],"#CCCCCC"),xlab="",ylab="",axes=axes,...)

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



plot.disProg.time.one <- function(x, k=1, title = "", ylim=NULL, legend=TRUE, ...) {
  observed   <- x$observed[,k]
  state      <- x$state[,k]
  alarm      <- x$alarm[,k]
  upperbound <- x$upperbound[,k]
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
    if (!is.null(x$control)) {
      method <- x$control$name
      disease <- x$control$data
      if (disease != "") { disease <- paste("of ",disease," ",sep="") }
      title(paste("Analysis ", as.character(disease), "using ", as.character(method),sep=""))
    }
  } else {
    title(title)
  }

  if (!is.null(x$aggr)) {
    points(upperboundx+tab,x$aggr,col=1)
  }
  
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
  observed <- x$observed
  nAreas <- ncol(observed)
  max <- max(observed)
  
  #multivariate time series
  if(nAreas > 1){
    #all areas in one plot 
    if(as.one){
      par(mfrow=c(1,1))

      ylim <- c(-1/20*max, max)
      matplot(observed,type="l",lty=1:nAreas,col=1:nAreas,ylim=c(0, 1.1*max),xlab="time",ylab="No. of Infected", ...)

      alarm <- apply(x$alarm,MARGIN=1,sum)>0
      state <- apply(x$state,MARGIN=1,sum)
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


plot.disProg <- function(obj,type,...) {
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
    plot.disProg.spacetime(obj,type,...)
    return(invisible())
  }
  #time plots
  if (time) {
    #In case observed ~ time, the units are aggregated
    if (justTime) {
      obj <- aggregateDisProg(obj)
    }
    plot.disProg.time(obj,type,...)
    return(invisible())
  }
}


create.disProg <- function(week, observed, state, neighbourhood=NULL, populationFrac=NULL,alarm=NULL,upperbound=NULL){
  namesObs <-colnames(observed)
  namesState <- colnames(observed)

  #univariate timeseries ?
  if(is.vector(observed))
    observed <- matrix(observed,ncol=1)
  if(is.vector(state))
    state <- matrix(state,ncol=1)
    
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
  
  #if(is.null(populationFrac)) 
  #
  if (nAreas ==1){
    populationFrac <- matrix(1,nrow=nObs, ncol=1)
  } 
  #if(is.null(neighbourhood) & (nAreas >1) )
  #  
  
  #labels for observed and state
  if(is.null(namesObs)){
    namesObs <- paste("observed", 1:nAreas, sep="")       
    namesState <- paste("state", 1:nAreas, sep="")  
  }
 
  dimnames(observed) <- list(NULL,namesObs)
  dimnames(state) <- list(NULL,namesState)

  if (is.null(alarm)) 
    alarm      <- ts(matrix(NA,nrow=dim(observed)[1],ncol=dim(observed)[2]),freq=frequency(observed),names=colnames(observed))
  if (is.null(upperbound))
  upperbound <- ts(matrix(NA,nrow=dim(observed)[1],ncol=dim(observed)[2]),freq=frequency(observed),names=colnames(observed))
  
  res <- list("week"=week, "observed"=observed, "state"=state, "neighbourhood"=neighbourhood, "populationFrac"=populationFrac,
              "alarm"=alarm,"upperbound"=upperbound)
  class(res) <- "disProg"
  return(res)
}

#library(maptools)
#library(surveillance)

data(measels.weser)
#map <- read.shape("~/Map/weserems.shp")
measels.weser$map <- readShapePoly("weserems.shp",IDvar="KRS_SCHL")
measels.weser$alarm      <- matrix(NA,nrow=dim(measels.weser$observed)[1],ncol=dim(measels.weser$observed))
measels.weser$upperbound <- matrix(NA,nrow=dim(measels.weser$observed)[1],ncol=dim(measels.weser$observed))

#read.shape("Z:/Surveillance/Map/weserems.shp")
#map <- read.shape("~/Map/weserems.shp")
#mappoly <- Map2poly(map, region.id = map$att.data$KRS_SCHL, quiet=TRUE)
#measels.weser$map <- mappoly
#plot(mappoly,axes=FALSE)

##Alternative data
#hepMale <- as.matrix(read.table("hepAmale.txt",header=TRUE,skip=1))
hepMale <- read.table("hepAmale.txt",header=TRUE,skip=1)
##Aggregate to month
year <- rep(2000+c(1,2,3,4,5,6),each=53)[1:dim(hepMale)[1]]
month <- rep(rep(1:12,times=c(rep(4,11),5)),length.out=dim(hepMale)[1])
mid <- month*1e4+year
hepMale <- as.matrix(aggregate(hepMale,by=list(mid),sum)[,-1])
hepMale[,1] <- 1:dim(hepMale)[1]

hepa.berlin <- create.disProg(week=hepMale[,1],observed=ts(hepMale[,-c(1,2,3)],freq=12),
                      state=matrix(0,dim(hepMale)[1],dim(hepMale)[2]-3))
hepa.berlin$map <-  readShapePoly("berlin.shp",IDvar="SNAME")
x <- aggregateDisProg(hepa.berlin)

test <- function() {
  source("gis.R")
  leg <- list(x=3300000,y=5770000,dx=0.4,dy=0.02,once=TRUE)
  col <- list(ncolors=100,use.color=TRUE)
  
  plot(measels.weser,legend=leg,type=observed ~ 1    | unit  )
  plot(measels.weser,legend=leg,type=observed ~ 1    | unit * time)
  plot(measels.weser,type=observed ~ time | unit)
  plot(measels.weser,type=observed ~ time + unit)
  plot(measels.weser,type=observed ~ time)


  plot(hepa.berlin,type=observed ~ 1    | unit )
  plot(hepa.berlin,type=observed ~ 1    | unit * time)
  plot(hepa.berlin,type=observed ~ time | unit)
  plot(hepa.berlin,type=observed ~ time + unit)
  plot(hepa.berlin,type=observed ~ time)

  #First attempt to do multivariate surveillance
  noUnits <- 12
  y2001 <- 1:noUnits
  y2002 <- (1*noUnits+1):(2*noUnits)
  y2003 <- (2*noUnits+1):(3*noUnits)
  y2004 <- (3*noUnits+1):(4*noUnits)
  y2005 <- (4*noUnits+1):(5*noUnits)
  y2006 <- (5*noUnits+1):dim(hepa.berlin$observed)[1]

  ab <- algo.bayes(hepa.berlin,control=list(range=c(y2005,y2006),b=2,w=1,alpha=0.001))
  plot(ab,type=observed ~ time | unit)
  plot(ab,type=observed ~ time + unit)
  plot(ab,type=observed ~ 1 |unit)
  plot(ab,type=observed ~ 1 |unit*time)
}







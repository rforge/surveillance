data("husO104Hosp")

#Extract the reporting triangle at a specific day
t.repTriangle <- as.Date("2011-07-04")

#Use 'void' nowcasting procedure (we just want the reporting triangle)
nc <- nowcast(now=t.repTriangle,when=t.repTriangle,
              dEventCol="dHosp",dReportCol="dReport",data=husO104Hosp,
              D=15,method="unif")

#Show reporting triangle 
reportingTriangle(nc)

#Perform Bayesian nowcasting assuming the delay distribution is stable over time
nc.control <- list(N.tInf.prior=structure("poisgamma",
                                mean.lambda=50,var.lambda=3000),
                                nSamples=1e2)

t.repTriangle <- as.Date("2011-06-10")
when <- seq(t.repTriangle-3,length.out=10,by="-1 day")
nc <- nowcast(now=t.repTriangle,when=when,
              dEventCol="dHosp",dReportCol="dReport",data=husO104Hosp,
              D=15,method="bayes.trunc",control=nc.control)

#Show time series and posterior median forecast/nowcast
plot(nc,xaxis.tickFreq=list("%d"=atChange,"%m"=atChange),
     xaxis.labelFreq=list("%d"=at2ndChange),xaxis.labelFormat="%d-%b",
     legend.opts=NULL, xlab="Time (days)",lty=c(1,1,1,1))

######################################################################
#Improved version with extra plotting
######################################################################

myHook <- function() {
    #Define some colors for the plotting. Could be put somewhere else.
    color <- c("violetred3","#2171B5","orange","blue","black","springgreen4")
        
    #Prolong line of last observation (this should go into the plot function
    idx <- nrow(x) - which.max(!is.na(rev(upperbound(x)))) + 1
    #Continue line from plot - use same style as stsplot_time1
    lines( idx+c(-0.5,0.5), rep(upperbound(x)[idx,],2),col=col[3],lwd=lwd[3],lty=lty[3])

    #Add the prediction intervals as bars (where not NA). Conf level
    #is found in x@control$alpha
    idxt <- which(apply(x@pi[1:nrow(x),1,],1,function(x) all(!is.na(x))))
    for (i in idxt) {
        lines( i+c(-0.3,0.3), rep(x@pi[i,,1],2),lty=1,col=color[3])
        lines( i+c(-0.3,0.3), rep(x@pi[i,,2],2),lty=1,col=color[3])
        lines( rep(i,each=2), x@pi[i,,],lty=2,col=color[3])
    }

    #Add now symbol
    points(curDate-range[1]+1,0,pch=10,col=control$col[6],cex=1.5)
    #Add nowcast symbol
    points(curDate-range[1]+1-control$safeguard,0,pch=9,col=control$col[3],cex=1.5)
    #Add this to the legend
    legend(x="right",c("Now","Nowcast horizon"),pch=c(10,9),col=control$col[c(6,3)],pt.cex=1.5)
    
    invisible()
}

#Improved plotting including the prediction interval quantiles using
#the hook function. Improve: Put this into the class' plot routine
plot(nc,xaxis.tickFreq=list("%d"=atChange,"%m"=atChange),
     xaxis.labelFreq=list("%d"=at2ndChange),xaxis.labelFormat="%d-%b",
     legend.opts=NULL, xlab="Time (days)",lty=c(1,1,1,1),hookFunc=myHook)

######### empty


nowCastRange <- seq(as.Date("2011-06-02"),as.Date("2011-07-04"),by="1 day"))


#Generate a list containing the nowcasts (instead of recomputing each time!)
nowcastList <- list()
for (i in 1:length(scoreRange)) {
  now <- scoreRange[i]
  nowcastList[as.character(now)] <- resObs[[as.character(now)]][["trunc"]]
}


t.repTriangle <- as.Date("2011-07-04")
seq(
nc <- nowcast(now=t.repTriangle,when=when,
              dEventCol="dHosp",dReportCol="dReport",data=husO104Hosp,
              D=15,method="bayes.trunc",control=nc.control)


    ## #Extract from nowcast object
    ## N.tInf.support <- x@control$N.tInf.support
    ## Ps <- x@predPMF
    ## when <- x@control$when
    ## dateRange <- epoch(x) 
    ## idxt <- which(dateRange %in% when)
    ## alpha <- x@control$alpha
    ## method <- 1
    ## browser()
    
    ## #Loop over all time points
    ## for (i in idxt) {
    ##     predPMF <- Ps[[as.character(dateRange[i])]][[method]]
    ##     x@upperbound[i,] <- median(N.tInf.support[which.max( cumsum(predPMF)>0.5)])
    ##     #Extract quantiles
    ##     quantileIdx <- c(which.max(cumsum(predPMF) >= alpha/2),which.max(cumsum(predPMF) >= 1-alpha/2))
    ##     x@pi[i,,] <- N.tInf.support[quantileIdx]
    ## }


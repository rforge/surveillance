#This routines read the log files and create figures

#The following variables have to be set

twinsPath <- "/Users/hoehle/Surveillance/surveillance/develop/twins/"
#path of the folder with the log files
projPath <- paste(twinsPath,"output/hepatitisA/",sep="")
#path of the folder where the figures are stored
figPath <- paste(twinsPath,"output/hepatitisA/",sep="")
#path of the folder with the data
dataPath <- paste(twinsPath,"data/",sep="")

#name of the data file
data.type<-"hepatitisA"
#path of the date file
data<-paste(dataPath, data.type, ".txt",sep="")
#path of the log files
log.file<-paste(projPath, data.type, ".log",sep="")
log.file2<-paste(projPath, data.type, "2.log",sep="")

#Variables from the estimation needed to read the log files and create figures
samplesize <- 200
season<-52
frequencies<-1

#define xlab for plots
time<-"weeks"



# functions

######################################################################
# Hoejsgaard function to wrap a plot and automatically generate the
# PDF file as well. Modified by hoehle to provide different page layouts
# for the ps and pdf files. 
#
# Parameters
#  expr - An expression, which upon evaluation generates the wanted
#         plot.
#  pre.par - Expression to evaluate before calling par. (e.g. mfcol stuff)
#  file - Basefile name .eps and .pdf versions are generated
#  path - Path
#  format- Layout format, currently fullA4 and halfA4 are supported
######################################################################

wrapPlot <- function(expr, file="Rfigure",pre.par=NULL,
                     path=".\\",format="halfA4",width=4,height=4){ 
  full.path      <- paste(path,file,sep="") 
  full.path.eps  <- paste(path,file,".eps",sep="") 
  full.path.pdf  <- paste(path,file,".pdf",sep="") 

  #Setup the new plot layout
  horizontal <- F
  onefile <- F
  paper <- "special"
  pointsize <- 11

  #If no global options are specified reset them to default
  if (!exists("wrapPlot.options")) {
    wrapPlot.options <- list(bty="l",family="Times")
  }
  
  switch (format,
          halfA4 = {
            width <- 5.5; height <-3.6
          },
          fullA4 = {
            width <- 11; height <- 3.6
          })
    
  #Open postscrip port
  postscript(file=full.path.eps,width=width,height=height,
             onefile=onefile,family=wrapPlot.options$family,
             pointsize=pointsize,
             paper=paper,horizontal=horizontal)

  eval(pre.par)
  par(cex=1.5,mex=0.6,cex.axis=1,mar=c(4.2, 4.8, 1.4,0.5),bty=wrapPlot.options$bty)
  eval(expr)
  dev.off()
  
  #PDF THING
  pdf(file=full.path.pdf,width=width,height=height,
             onefile=onefile,family=wrapPlot.options$family,pointsize=pointsize,
             paper=paper)
  eval(pre.par)
  par(cex=1.5,mex=0.6,cex.axis=1,mar=c(4.2, 4.8,1.4,0.5),bty=wrapPlot.options$bty)
  eval(expr)

  #Close
  invisible(dev.off())
}


#Plot of the data Z and the posterior means of X and Y over time
plot.pois <- function(m.results,n,xlab="") {
  plotorder <- c(expression(Z),expression(X),expression(Y))
  plotcols <- c(1,"blue","red")
  lwd <- c(1,3,3)
  ymax <- 5/4*max(m.results[[paste(plotorder[1])]])
  plot(0:(n-1),m.results[[paste(plotorder[1])]],type="s",col=plotcols[1],
       ylim=c(0,ymax),xlab=xlab,ylab="No. of cases",lwd=lwd[1])

  for (i in 2:length(plotorder)) {
    lines(1:(n-1),m.results[[paste(plotorder[i])]][2:n],type="s",col=plotcols[i],lwd=lwd[i])
  }
  legend(0,ymax,paste(plotorder),lwd=lwd,col=plotcols,horiz=T,y.intersp=0)
}



# Functions to make list of Z and the means of X,Y and omega
make.pois <- function(dataFile,logFile,n) {
  m<-list()
  m$Z <- scan(dataFile)[-1]
  m$X <- numeric(n)
  m$Y <- numeric(n)
  m$omega <- numeric(n)
  # Read means at each time instance
  Vars <- c("X","Y","omega") 
    for (t in 1:n) {
      for (v in Vars) {
        m[[v]][t] <- logFile[,paste(v,".",t,".",sep="")]
      }
    }
  return(m)
}


#Function to open log file.
read.logfile <- function(logFileName) {
  log <- read.table(logFileName,header=T,na.strings=c("NaN","-NaN"))
  #Read acceptance MCMC information
  acc <- read.table(paste(logFileName,".acc",sep=""),col.names=c("name","RWSigma","acc"))
  rownames(acc) <- acc[,1]
  acc <- acc[,-1]
  return(return(list(log=log,acc=acc)))
}

#Function to open log file with means of X,S,Y.
read.logfile2 <- function(logFileName) {
  log <- read.table(logFileName,header=T,na.strings=c("NaN","-NaN"))
  return(log)
}



# makes list of gamma, zeta and nu
make.nu <- function(dataFile,logFile,frequencies,season,n,samplesize) {
  m<-list()
  basefrequency<-2*pi/season
  for (j in 0:(2*frequencies)) {
        m$gamma[[j+1]] <- numeric(samplesize)
        m[["gamma"]][[j+1]] <- logFile[,paste("gamma",".",j,".",sep="")]
  }
  m$zeta<-list()
  for (t in 1:n) {
    m$zeta[[t]]<-m$gamma[[1]]
    for(j in 1:frequencies){
      m$zeta[[t]] <- m$zeta[[t]] + m$gamma[[2*j]]*sin(basefrequency*j*(t-1)) + m$gamma[[2*j+1]]*cos(basefrequency*j*(t-1)) 
    }
  }
  m$nu<-list()
  for (t in 1:n) {
    m$nu[[t]]<-exp(m$zeta[[t]])
  }
  return(m)
}


#Function to plot median, and quantiles over time for m.par (m.par is list of n vectors, x is time)
plot.tms<-function(x,m.par,xlab="",ylab="",ylim=F,...){
  m<-list()
  n<-length(m.par)
  m$median<-numeric(n)
  for (t in 1:n) {
   m$median[t]<- median(m.par[[t]])
   m$q025[t]<- quantile(m.par[[t]],0.025)
   m$q975[t]<- quantile(m.par[[t]],0.975)
  }
  if(!ylim){
  ymin<-min(m$q025)
  ymax<-max(m$q975)
  ylim=c(ymin,ymax)
  }

  plot(x-1,m$q975[x],type="l",col="red",main="",xlab=xlab,ylab=ylab,ylim=ylim,...) 
  lines(x-1,m$median[x],type="l")
  lines(x-1,m$q025[x],type="l",col="red")

}

#function make a list of lambda
make.lambda <- function(dataFile,logFile,n,samplesize) {
  lambda<-list()
  lambda[[1]]<-numeric(samplesize)
    for (t in 2:n) {
        lambda[[t]] <- numeric(samplesize)
        lambda[[t]] <- logFile[,paste("lambda",".",t,".",sep="")]
    }
  return(lambda)
}

#function make a list of the  breakpont probabilities
make.bp <- function(dataFile,logFile,n) {
    bp<-numeric(n+1)
    for (t in 2:(n+1)) {
        bp[t] <- logFile[,paste("bp.",t,".",sep="")]
    }
  return(bp)
}


#make figures

  #Read the log files
  results <- read.logfile(log.file)
  results2 <- read.logfile2(log.file2)

  #Read n
  n<-scan(data)[1]

  #Make list of X,Y,Z,omega means of results2
  m.results <-make.pois(data,results2,n)

  nu<-0
  #Make list of results of  gamma, zeta and nu
  nu<-make.nu(data,results$log,frequencies,season,n,samplesize)

  lambda<-0
  #Make list of results of lambda
  lambda<-make.lambda(data,results$log,n,samplesize)
  #Make list of bp
  bp<-make.bp(data,results2,n)

#define xlab for plots
time<-"weeks"

#Plot the data
d<-scan(data)
d<-d[-1]
wrapPlot(expression(plot(d,type="l",ylim=c(0,max(d)),main="",xlab=time,ylab="No. of cases")),          
         file=paste(data.type,"-plot", sep=""),path=figPath,
           format="fullA4")


#Plot of median and pointwise credibility intervals of the lambda over time
wrapPlot(expression(plot.tms(2:n,lambda,xlab=time),lines(1:(n-1),rep(1,n-1),lty=2)),       
         file="tms-lambda",path=figPath,
           format="fullA4")


#plot for p(lambda > 1)
lambdage1<-numeric(n-1)
for(t in 1:(n-1)){
  lambdage1[t]<-length(lambda[[t+1]][lambda[[t+1]]>1])/samplesize
}
wrapPlot(expression(plot(1:(n-1),lambdage1,type="l",main="",xlab=time,ylab="Posterior Probability")),          
         file="lambdage1",path=figPath,
           format="fullA4")


#unconditioned theta probabilities
wrapPlot(expression(plot(0:n,c(0,bp[2:(n+1)]),type="h",main="",xlab=time,ylab="")),          
         file="theta",path=figPath,
           format="fullA4")



# Plots of the data Z and the posterior means of X and Y over time
  wrapPlot(expression(plot.pois(m.results,n,xlab=time)),
           file="xyz",path=figPath,
           format="fullA4")


# trace plots
for(j in 0:2*frequencies){
  wrapPlot(expression(plot(nu$gamma[[j+1]],type="l",ylab=paste("gamma",j,sep=""))),
           file=paste("traj-gamma-",j,sep=""),path=figPath,
           format="halfA4")
}
  wrapPlot(expression(plot(results$log$K,type="l",ylab=expression(K))),
           file="traj-K",path=figPath,
           format="halfA4")
  wrapPlot(expression(plot(results$log$xilambda,type="l",ylab=expression(xi))),
           file="traj-xi",path=figPath,
           format="halfA4")
  wrapPlot(expression(plot(results$log$psi,type="l",ylab=expression(psi))),
           file="traj-psi",path=figPath,
           format="halfA4")



#Autokorrelation
wrapPlot(expr=expression(
    acf(results$log$K,lag.max = 500,main="",xlab=expression(K)),
    acf(results$log$psi,lag.max = 500,main="",xlab=expression(psi))),
    pre.par=expression(par(mfcol=c(1,2))),
    file="autocorrelation",path=figPath,
    format="fullA4")

#Plot for nu
  wrapPlot(expression(plot.tms(2:n,nu$nu,xlab=time)),
           file="tms-nu",path=figPath,
          format="fullA4")

#histogram of the distribution of K
  wrapPlot(expr=expression(
    hist(results$log$K,main="",xlab=expression(K),prob=T,breaks=seq(-0.5,max(results$log$K)+0.5,1))),
    file="histogram-K",path=figPath,
    format="fullA4")

#histogram of the distribution of psi
  wrapPlot(expr=expression(
    hist(results$log$psi,main="",xlab=expression(psi),prob=T,nclass=50)),
    file="histogram-psi",path=paste(figPath,"",sep=""),
    format="fullA4")

#histogram of the predictive distribution
  wrapPlot(expr=expression(
    hist(results$log$Znp1,main="",xlab=expression(Z[n+1]),prob=T,breaks=seq(-0.5,max(results$log$Znp1)+0.5,1))),
    file="histogram-Znp1",path=figPath,
    format="fullA4")


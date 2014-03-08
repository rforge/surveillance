\name{display-methods}
\alias{plot,sts,missing-method}
%\alias{print}
%\alias{print,sts-method}
\alias{show,sts-method}
\alias{plot.sts.time}
\alias{plot.sts.time.one}
\alias{plot.sts.spacetime}
\alias{plot.sts.alarm}
%Plot specific helper functions
\alias{atChange}
\alias{at2ndChange}
\alias{atMedian}

\title{Display Methods for Surveillance Time-Series Objects}
\description{
  Display methods for objects of class \code{"sts"}.
}

\section{Usage}{
  plot(x,y,type,...)
  print(x,...)
}

\section{Arguments}{
\describe{
  \item{x}{an object of class \code{"sts"}}
  \item{y}{missing}
  \item{type}{a formula specifying the plot type, several options are
    possible:
    \describe{
	\item{\code{observed ~ time}}{The observations in \code{x} are
	  aggregated over units and the resulting univariate time-series
	  is plotted. The plotting is done by the function
	  \code{plot.time.sts}, which takes the same arguments as the
	  \code{\link{plot.survRes}} function.}
	\item{\code{observed ~ time | unit}}{shows \code{dim(x)} plots
	  with each showing the time-series of one observational
	  unit. The actual plotting is done by the function 
	  \code{plot.time.sts.one}}
	\item{\code{observed ~ 1 | unit}}{for each unit the counts are
	  aggregated over time and a map illustrating the counts is
	  shown. The column names of the \code{x@observed} object are
	  used to label the entries of the \code{x@map}. Regions with an
	  alarm are shaded.}
	\item{\code{observed ~ 1 | unit * time}}{an animation consisting
	  of \code{nrow(x)} frames is generated. Each frame contains the
	  number of counts per region for the current row in the
	  \code{observed} matrix. It is possible to redirect the output
	  into files, e.g. to generate an animated GIF. See the
	  examples.}
	\item{\code{alarm ~ time}}{Generates a so called alarmplot for a
	  multivariate \code{sts} object. For each time point and each
	  series it is shown whether there is an alarm. In case of
	  hierarchical surveillance the user can pass
	  an additional argument \code{lvl}, which is a vector of the
	  same length as rows in \code{x} specifying for each time series
	  its level.
	}
      }
    }
    \item{\dots}{further arguments passed to or from other methods:
      in case of plotting these are passed to \code{\link{plot}},
      in case of printing these are passed to \code{\link{print.default}}}
  }
}
\details{
  The plotting of time-series plots relies on two internal functions
  with \code{plot.sts.time.one} being the work-horse. Its arguments are
  (almost) similiar to \code{\link{plot.survRes}}. \code{k} is the
  column to plot.

  \preformatted{plot.sts.time(x,
    type,method=x@control$name,disease=x@control$data,same.scale=TRUE,
    par.list=list(mfrow=magic.dim(nAreas),mar=par()$mar),...)}
  
  \preformatted{plot.sts.time.one <- function(x, k=1,
    domany=FALSE,ylim=NULL, axes=TRUE,
    xaxis.tickFreq=list("%Q"=atChange),xaxis.labelFreq=xaxis.tickFreq,
    xaxis.labelFormat="%G\n\n%OQ", epochsAsDate=x@epochAsDate,
    xlab="time", ylab="No. infected", main=NULL, type="s",lty=c(1,1,2),
    col=c(NA,1,4),lwd=c(1,1,1),
    outbreak.symbol = list(pch=3, col=3, cex=1,lwd=1),
    alarm.symbol=list(pch=24, col=2, cex=1,lwd=1),
    legend.opts=list(x="top", legend=NULL,lty=NULL,pch=NULL,col=NULL),
    dx.upperbound=0L,hookFunc=function() {}, ...)}

  Note that the \code{hookFunc} is called after all the basic plotting
  has be done, i.e. it is not possible to control formatting with this
  function. Instead, one can put elements into \code{par.list}, which is
  delivered to a call of \code{par} before each plot.

  In case the epochs of the \code{sts} object are \code{Date} objects it
  is possible to obtain very flexible formatting of the x-axis and its
  annotations using the \code{xaxis.tickFreq}, \code{xaxis.labelFreq}
  and \code{xaxis.labelFormat}. The two first are named list containing
  pairs with the \emph{name} being a \code{\link{strftime}} single
  conversion specification and the second part is a function which based
  on this conversion returns a subset of the rows in the \code{sts}
  objects. The subsetting function has the following header:
  \code{function(x,xm1)}, where \code{x} is a vector containing
  the result of applying the conversion in \code{name} to the epochs of
  the \code{sts} object and \code{xm1} is the scalar result when
  applying the conversion to the natural element just before the first
  epoch.  Three predefined subsetting functions exist:
  \code{atChange}, \code{at2ndChange}  and \code{atMedian}, which 
  are used to make a tick at each (each 2nd for \code{at2ndChange})
  change and at the median index computed on all having the same value,
  respectively. For example: 
  \preformatted{
    atChange <- function(x,xm1) which(diff(c(xm1,x)) != 0)
    at2ndChange <- function(x,xm1) {which(diff(c(xm1,x) \%/\% 2) != 0)}
    atMedian <- function(x,xm1) tapply(seq_len(length(x)), INDEX=x, quantile, prob=0.5,type=3)
  }
  By defining own functions here, one can obtain an arbitrary degree of
  flexibility. 
  
  Finally, \code{xaxis.labelFormat} is a \code{\link{strftime}}
  compatible formatting string., e.g. the default value is
  \code{\"\%G\\n\\n\%OQ\"}, which means ISO year and quarter (in roman
  letters) stacked on top of each other.
  
  
  For spacetime plots the following internal function does all the work:

  \preformatted{plot.sts.spacetime(x,type,legend=NULL,opts.col=NULL,
    labels=TRUE,wait.ms=250,cex.lab=0.7,verbose=FALSE,
    dev.printer=NULL,...)}

  Alarmplot
  \preformatted{plot.sts.alarm <- function(x, lvl=rep(1,nrow(x)),
    ylim=NULL, xaxis.units=TRUE,
    xaxis.tickFreq=list("%Q"=atChange),xaxis.labelFreq=xaxis.tickFreq,
    xaxis.labelFormat="%G\n\n%OQ",epochsAsDate=x@epochAsDate,
    xlab="time", main=NULL, type="hhs",lty=c(1,1,2),col=c(1,1,4),
    outbreak.symbol = list(pch=3, col=3, cex=1, lwd=1),
    alarm.symbol=list(pch=24, col=2,cex=1,lwd=1),
    cex=1,cex.yaxis=1,...)}
  
  \code{print} is the method for printing \code{sts} objects.

}

\value{

The methods are called for their side-effects.

}

\seealso{
  \code{\link{plot.survRes}}
}

\examples{
data("ha.sts")
print(ha.sts)
plot(ha.sts, type= observed ~ time)
plot(ha.sts, type= observed ~ time | unit)
plot(ha.sts, type= observed ~ 1 | unit)
plot(ha.sts[1:20,1:2], type= observed ~ 1 | unit)

#Hook function
hookFunc <- function() { grid(NA,NULL,lwd=1) }
plot(ha.sts, type= observed ~ time | unit, hookFunc=hookFunc)

#Use ISO8601 date formatting (see ?strptime) and no legend
data("salmNewport")
plot(aggregate(salmNewport,by="unit"),xlab="time (weeks)",
         xaxis.tickFreq=list("\%m"=atChange,"\%G"=atChange),
         xaxis.labelFreq=list("\%G"=atMedian),xaxis.labelFormat="\%G",
         legend.opts=NULL)

\dontrun{
# space-time animation
plot(aggregate(ha.sts,nfreq=13), type= observed ~ 1 | unit * time)

#Configure a png device printer to save the frames
dev.printer <- list(device=png, extension=".png", width=640, height=480,
                    name=file.path(tempdir(),"berlin"))

#Do the animation (without extra sleeping time between frames)
plot(aggregate(ha.sts,nfreq=13), type = observed ~ 1 | unit * time,
     wait.ms=0, dev.printer=dev.printer)

#Use ImageMagick (you might have to adjust the path to 'convert')
system(paste("convert -delay 50 ",dev.printer$name,
           "*.png ", dev.printer$name, "-animated.gif",sep=""))
}
}

\keyword{ts}
\keyword{hplot}

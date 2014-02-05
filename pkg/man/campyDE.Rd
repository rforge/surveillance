\name{campyDE}
\alias{campyDE}
\docType{data}
\title{Cases of Campylobacteriosis and Absolute Humidity in Germany 2002-2011}
\description{
  Weekly number of reported campylobacteriosis cases in Germany
  2002-2011 together with the corresponding absolute humidity (in g/m^3)
  that week. The absolute humidity was computed according to the
  procedure by Dengler (1997) using the means of representative weather
  station data from the German Climate service.
}
\usage{
data(campyDE)
}
\format{
  A \code{data.frame} containing the following columns
  \describe{
    \item{\code{date}}{\code{Date} instance containing the monday of the
      reporting week.}
    \item{\code{case}}{Number of reported cases that week.}
    \item{\code{state}}{Boolean indicating whether there is external knowledge
      about an outbreak that week}
    \item{\code{hum}}{Mean absolute humidity (in g/m^3) of that week as
      measured by a single representative weather station.}
    \item{\code{l1.hum}-\code{l5.hum}}{Lagged version (lagged by 1-5) of
      the \code{hum} covariate.}
    \item{newyears}{Boolean indicating whether the reporting week
      corresponds to the first two weeks of the year (TRUE) or not
      (FALSE).}
    \item{christmas}{Boolean indicating whether the reporting week
      corresponds to the last two weeks of the year (TRUE) or not (FALSE).}
  }
}
\source{
  The data on campylobacteriosis cases are queried from the Survstat@RKI
  database of the German Robert Koch Institute
  (\url{http://www3.rki.de/SurvStat/}).

  Data for the computation of absolute humidity were obtained from the
  German Climiate Service (Deutscher Wetterdienst), Climate data of
  Germany, available at \url{http://www.dwd.de}.

  A complete data description and an analysis of the data can be found in:
  
  Bayesian model algorithm for monitoring reported cases of
  campylobacteriosis in Germany (2013), Manitz J and Höhle M,
  Biometrical Journal, 55(4), pp. 509–526.
}  
\examples{
  data("campyDE")

  \dontrun{
  #Trial run code. To be made an actual working example.
  #load("../data/campyDE.RData")
  source("../R/boda.R")

  #Make an sts object from the data.frame
  cam.sts <- new("sts",epoch=as.numeric(campyDE$date),observed=campyDE$case, state=campyDE$state, epochAsDate=TRUE)
  plot(cam.sts)

  # define monitoring period
#  range <- which(epoch(cam.sts)>=as.Date("2007-01-01"))
  range <- which(epoch(cam.sts)>=as.Date("2011-12-10"))
#  range <- which(epoch(cam.sts)>=as.Date("2011-11-01"))

  control <- list(range=range, co.arg=NULL, trend=TRUE, season=TRUE,prior='iid', alpha=0.025, mc.betaT1=100, mc.yT1=10)
#debug("bodaFit")
  #Apply the boda algorithm (NOTE: requires the INLA package to be installed)
  cam.boda1 <- boda(cam.sts, control=control)

  plot(cam.boda1,xlab='time [weeks]', ylab='No. reported',dx.upperbound=0)

  } 
}
\keyword{datasets}

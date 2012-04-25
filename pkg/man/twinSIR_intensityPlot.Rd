\encoding{latin1}
\name{twinSIR_intensityPlot}
\alias{intensityPlot}
\alias{plot.twinSIR}

\title{
Plotting Paths of Infection Intensities for \code{twinSIR} Models
}

\description{
Function to plot the values of the total infection intensity, its epidemic
proportion and its endemic proportion along the evolution of the epidemic.
}

\usage{
intensityPlot(x, type = c("overall", "individual"), 
              what = c("epidemic proportion", "endemic proportion",
                       "total intensity"),
              theta = NULL, plot = TRUE, add = FALSE, rug.opts = list(), ...)

\method{plot}{twinSIR}(x, type = c("overall", "individual"),
     what = c("epidemic proportion", "endemic proportion", "total intensity"),
     theta = coef(x), plot = TRUE, add = FALSE, rug.opts = list(), ...)
}

\arguments{
  \item{x}{
    an object of class \code{"\link{simEpidata}"} or \code{"\link{twinSIR}"}.
  }
  \item{type}{
    single character: \code{"overall"} or \code{"individual"}.  Partial matching
    is applied.  Determines whether lines for all individual infection
    intensities should be drawn or their sum only.
  }
  \item{what}{
    single character: \code{"epidemic proportion"}, \code{"endemic proportion"}
    or \code{"total intensity"}.  Partial matching is applied.  Determines
    whether to plot the path of the total intensity \eqn{\lambda(t)} or its 
    epidemic or endemic proportions
    \eqn{\frac{e(t)}{\lambda(t)}}{e(t)/lambda(t)} or
    \eqn{\frac{h(t)}{\lambda(t)}}{h(t)/lambda(t)}. 
  }
  \item{theta}{
    numeric vector of model coefficients.  If \code{x} is of class
    \code{"twinSIR"}, then \code{theta = c(alpha, beta)}, where \code{beta}
    consists of the coefficients of the piecewise constant log-baseline function
    and the coefficients of the endemic (\code{cox}) predictor.  If \code{x} is
    of class \code{"simEpidata"}, then \code{theta = c(alpha, 1, betarest)}, 
    where 1 refers to the (true) log-baseline used in the simulation and 
    \code{betarest} is the vector of the remaining coefficients of the endemic 
    (\code{cox}) predictor.
    The default (\code{NULL}) means that the fitted or true parameters,
    respectively, will be used.
  }
  \item{plot}{
    logical indicating if a plot is desired, defaults to \code{TRUE}.
    Otherwise, only the data of the plot will be returned.  Especially with
    \code{type = "individual"} and many individuals one might e.g.\ consider to 
    plot a subset of the individual intensity paths only or do some further
    calculations/analysis of the infection intensities.
  }
  \item{add}{
    logical.  If \code{TRUE}, plots are added to current one, using
    \code{lines}.
  }
  \item{rug.opts}{
    either a list of arguments passed to the function \code{\link{rug}} or
    \code{NULL} (or \code{NA}), in which case no \code{rug} will be plotted.
    By default, the argument \code{ticksize} is set to 0.02 and \code{quiet}
    is set to \code{TRUE}.  Note that the argument \code{x}, which contains the
    locations for the \code{rug} is fixed internally and can not be modified.
    The locations of the rug are the time points of infections.
  }
  \item{\dots}{
    further graphical parameters passed to the function \code{\link{matplot}},
    e.g.\ \code{lty}, \code{lwd}, \code{col}, \code{xlab}, \code{ylab} and
    \code{main}.  Note that the \code{matplot} arguments \code{x}, \code{y},
    \code{type} and \code{add} are implicit and can not be specified here.
  }
}

\value{
  numeric matrix with the first column \code{"stop"} and as many rows as there
  are \code{"stop"} time points in the event history \code{x}.  The other 
  columns depend on the argument \code{type}: if \code{type = "overall"} there 
  is only one other column named \code{what}, which contains the values of 
  \code{what} at the respective \code{"stop"} time points.  Otherwise, if
  \code{type = "individual"}, there is one column for each individual, each of
  them containing the individual \code{what} at the respective \code{"stop"}
  times points.
}

\references{
  \enc{Höhle}{Hoehle}, M. (2009),
  Additive-Multiplicative Regression Models for Spatio-Temporal
  Epidemics, Biometrical Journal, 51(6):961-978.
}

\author{
Sebastian Meyer
}

\seealso{
\code{\link{twinSIR}} or \enc{Höhle}{Hoehle} (2009) for a more detailed
description of the intensity model.
}

\examples{
data("fooepidata")
data("foofit")

# an overview of the evolution of the epidemic
plot(fooepidata)

# overall total intensity
plot(foofit, what="total")

# overall epidemic proportion
head(plot(foofit, what="epidemic"))

# add the inverse overall endemic proportion = 1 - epidemic proportion
head(plot(foofit, what="endemic", add=TRUE, col=2))
legend("right", legend="endemic proportion \n(= 1 - epidemic proportion)",
       lty=1, col=2, bty="n")

# individual intensities
tmp <- plot(foofit, type="individual", what="total",
    col=rgb(0,0,0,alpha=if(getRversion() < "2.7.0") 1 else 0.1),
    main=expression("Individual infection intensities" *
                    lambda[i](t) == Y[i](t) \%.\% (e[i](t) + h[i](t))))
str(tmp)

# and only for individuals 3 and 99
matplot(x= tmp[,1], y=tmp[,1+c(3,99)], type="S", ylab="infection intensity",
        xlab="time", main=expression("Paths of the infection intensities" *
                                  lambda[3](t) * "  and  " * lambda[99](t)))
legend("topright", legend=paste("Individual", c(3,99)), col=c(1,2), lty=c(1,2))
}

\keyword{hplot}
\keyword{aplot}
\keyword{dplot}
\keyword{methods}

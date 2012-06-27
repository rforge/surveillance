%%% TODO: could be removed (internal unexported function)
\encoding{latin1}
\name{twinSIR_intensityplot_internal}
\alias{intensityplot_twinSIR}

\title{
Plotting Paths of Infection Intensities for \code{twinSIR} Models
}

\description{
Internal function to plot the values of the total infection intensity,
its epidemic proportion and its endemic proportion along the evolution
of the epidemic. It is called by the \code{\link{intensityplot}} methods
for classes \code{"twinSIR"} and \code{"simEpidata"}.
}

\usage{
intensityplot_twinSIR(model,
which = c("epidemic proportion", "endemic proportion", "total intensity"),
aggregate = TRUE, theta = NULL,
plot = TRUE, add = FALSE, rug.opts = list(), ...)
}

\arguments{
  \item{model}{
    the \code{model} component of a fitted \code{twinSIR}, or, more
    generally, the result of calling \code{getModel}, which is an internal
    function also applicable to objects of class
    \code{"\link{simEpidata}"} (simulated \code{twinSIR} epidemic).
  }
  \item{which}{
    \code{"epidemic proportion"}, \code{"endemic proportion"},
    or \code{"total intensity"}.  Partial matching is applied.  Determines
    whether to plot the path of the total intensity \eqn{\lambda(t)} or its 
    epidemic or endemic proportions
    \eqn{\frac{e(t)}{\lambda(t)}}{e(t)/lambda(t)} or
    \eqn{\frac{h(t)}{\lambda(t)}}{h(t)/lambda(t)}. 
  }
  \item{aggregate}{
    logical. Determines whether lines for all individual infection
    intensities should be drawn (\code{FALSE}) or their sum only
    (\code{TRUE}, the default).
  }
  \item{theta}{
    numeric vector of model coefficients.  If the \code{model} stems
    from a fitted 
    \code{"twinSIR"}, then \code{theta = c(alpha, beta)}, where \code{beta}
    consists of the coefficients of the piecewise constant log-baseline function
    and the coefficients of the endemic (\code{cox}) predictor.
    If the \code{model} stems from a simulated twinSIR epidemic, then
    \code{theta = c(alpha, 1, betarest)}, 
    where 1 refers to the (true) log-baseline used in the simulation and 
    \code{betarest} is the vector of the remaining coefficients of the endemic 
    (\code{cox}) predictor.
    The default (\code{NULL}) means that the fitted or true parameters,
    respectively, will be used.
  }
  \item{plot}{
    logical indicating if a plot is desired, defaults to \code{TRUE}.
    Otherwise, only the data of the plot will be returned.  Especially with
    \code{aggregate = FALSE} and many individuals one might e.g.\ consider to 
    plot a subset of the individual intensity paths only or do some further
    calculations/analysis of the infection intensities.
  }
  \item{add}{
    logical.  If \code{TRUE}, paths are added to the current plot, using
    \code{lines}.
  }
  \item{rug.opts}{
    either a list of arguments passed to the function \code{\link{rug}}, or
    \code{NULL} (or \code{NA}), in which case no \code{rug} will be plotted.
    By default, the argument \code{ticksize} is set to 0.02 and \code{quiet}
    is set to \code{TRUE}.  Note that the argument \code{x} of the
    \code{rug()} function, which contains the
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
  are \code{"stop"} time points in the event history (\code{model}).  The other 
  columns depend on the argument \code{aggregate}: if \code{TRUE}, there 
  is only one other column named \code{which}, which contains the values of 
  \code{which} at the respective \code{"stop"} time points.  Otherwise, if
  \code{aggregate = FALSE}, there is one column for each individual, each of
  them containing the individual \code{which} at the respective \code{"stop"}
  time points.
}

\author{
Sebastian Meyer
}

\seealso{
\code{\link{intensityplot.twinSIR}} and
\code{\link{intensityplot.simEpidata}},
which both call this function internally.
}

\keyword{internal}
\keyword{aplot}

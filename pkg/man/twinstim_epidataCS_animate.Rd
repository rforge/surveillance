\encoding{latin1}
\name{twinstim_epidataCS_animate}
\alias{animate.epidataCS}

\title{
Spatio-Temporal Animation of a Continuous-Time Continuous-Space Epidemic
}

\description{
  Function for the animation of epidemic data, i.e.\ objects inheriting from 
  class \code{"epidataCS"}.  There are two types of animation, see argument 
  \code{time.spacing}.  Besides the direct plotting in the \R session, it is 
  also possible to generate a sequence of graphics files to create animations 
  outside \R.  The function works in the framework of the contributed \R package
  \pkg{animation} and respects the \code{\link[animation]{ani.options}}
  \code{"interval"} and \code{"nmax"}.
}

\usage{
\method{animate}{epidataCS}(object, interval = c(0,Inf), time.spacing = NULL,
    legend.opts = list(), timer.opts = list(), pch = 15:18, col.current = "red",
    col.I = "#C16E41", col.R = "#B3B3B3", col.influence = "#FEE0D2", ...)
}

\arguments{
  \item{object}{
    an object inheriting from class \code{"epidataCS"}.
  }
  \item{interval}{Time interval for the simulation.}
  \item{time.spacing}{
    time interval for the animation steps.  If \code{NULL} (the default), the
    events are plotted one by one with pauses of \code{sleep} seconds.  Thus, 
    it is just the \emph{ordering} of the events, which is shown.  To plot
    the appearance of events proportionally to the exact time line,
    \code{time.spacing} can be set to a numeric value indicating the period of
    time between consecutive plots.  Then, for each time point in
    \code{seq(0, end, by = time.spacing)} the current state of the epidemic can
    be seen and an additional timer indicates the current time
    (see \code{timer.opts} below).  The argument \code{sleep} will be the
    artificial pause in seconds between two of those time points.
  }  
  \item{pch, col}{
    vectors of length equal to the number of event types specifying the
    point symbols and colors for events to plot (in this order).
    The vectors are recycled if necessary.
  }
  \item{legend.opts}{
    either a list of arguments passed to the \code{\link{legend}} function or
    \code{NULL} (or \code{NA}), in which case no legend will be plotted.  All 
    necessary arguments have sensible defaults and need not be
    specified.
  }
  \item{timer.opts}{
    either a list of arguments passed to the \code{\link{legend}} function or
    \code{NULL} (or \code{NA}), in which case no timer will be plotted.  All 
    necessary arguments have sensible defaults and need not be specified, i.e.\
    \describe{
      \item{\code{x}:}{\code{"bottomright"}}
      \item{\code{title}:}{\code{"time"}}
      \item{\code{box.lty}:}{\code{0}}
      \item{\code{adj}:}{\code{c(0.5,0.5)}}
      \item{\code{inset}:}{\code{0.01}}
      \item{\code{bg}:}{\code{"white"}}
    }
    Note that the argument \code{legend}, which is the current time of the
    animation, can not be modified.
  }
  \item{col.current}{Color of events when occuring}
  \item{col.I}{Color once infectious}
  \item{col.R}{Color event has once 'Recovered', if \code{NULL}
  (default) then to not shown.}
  \item{col.influence}{Color with which influence region is drawn. Use
  \code{NA} if no influence regions should be drawn.}
  \item{\dots}{
    further graphical parameters passed to the basic call of \code{plot}, e.g.\
    \code{las}, \code{cex.axis} (etc.) and \code{mgp}.
  }
}

%\value{
%  invisibly returns \code{NULL}.
%}

\author{
Sebastian Meyer with documentation by Michael H\enc{ö}{oe}hle
}

\seealso{
The plot is based on the return value of \code{\link{summary.epidataCS}}.

\code{\link{plot.epidataCS}} for plotting the numbers of events by time
(aggregated over space) or the locations of the events in the
observation region \code{W} (aggregated over time).

The contributed \R package \pkg{animation}.
}

\examples{
# TODO ...
}

\keyword{hplot}
\keyword{dynamic}
\keyword{spatial}

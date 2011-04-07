\name{twinstim_epidataCS_animate}
\alias{animate.epidataCS}
\alias{animate.summary.epidataCS}

\title{
Spatio-Temporal animation of an continuous-time continuous-space epidemic
}

\description{
  Function for the animation of epidemic data, i.e.\ objects inheriting from 
  class \code{"epidataCS"}.  There are two types of animation, see argument 
  \code{time.spacing}.  Besides the direct plotting in the \R session, it is 
  also possible to generate a sequence of graphics files to create animations 
  outside \R.
}

\usage{
\method{animate}{animate.epidataCS}(object, interval = c(0,Inf), time.spacing = NULL,
    legend.opts = list(), timer.opts = list(), pch = 15:18, col.current = "red",
    col.I = getOption("epicolor")$I, col.R = getOption("epicolor")$R,
    col.influence = "#FEE0D2", ...)
}

\arguments{
  \item{object}{
    an object inheriting from class \code{"epidataCS"}.
  }
  \item{interval}{}
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
Sebastian Meyer, Documentation by Michael H\enc{oe}{ö}hle
}

\seealso{
\code{\link{summary.epidataCS}} for the data, on which the plot is based.
\code{\link{plot.epidataCS}} for plotting the evolution of an epidemic by
the numbers of susceptible, infectious and removed individuals.

%The contributed \R package \code{animation}.
}

\examples{
}

\keyword{hplot}
\keyword{dynamic}
\keyword{spatial}

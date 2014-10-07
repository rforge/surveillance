\name{pit}
\alias{pit}

\title{
Non-Randomized Version of the PIT Histogram (for Count Data)
}

\description{
  See Czado et al. (2009).
}

\usage{
pit(x, pdistr, J = 10, relative = TRUE, ..., plot = list())
}

\arguments{
  \item{x}{
    numeric vector representing the observed counts.
  }
  \item{pdistr}{
    the predictive CDF for the observations \code{x}, i.e.,
    a vectorized \code{function (x, ...)}.
    If the observations \code{x} have different predictive
    distributions, these may be provided in a list of the same length as
    the vector \code{x}.
  }
  \item{J}{
    the number of bins of the histogram.
  }
  \item{relative}{
    logical indicating if relative frequency or the density should be plotted.
  }
  \item{\dots}{
    arguments for \code{pdistr}. Ignored if \code{pdistr} is a list.
  }
  \item{plot}{
    a list of arguments for \code{\link{plot.histogram}} or \code{NULL}
    in which case no plot will be produced.
  }
}

\value{
  an object of class \code{"histogram"} (see \code{\link{hist}}).
  It is returned invisibly if a plot is produced.
}

\references{
  Czado, C., Gneiting, T. & Held, L. (2009):
  Predictive model assessment for count data.
  \emph{Biometrics}, \bold{65}, 1254-1261.
}

\author{
  Michaela Paul and Sebastian Meyer
}

\examples{
## Simulation example of Czado et al. (2009, Section 2.4)
set.seed(100)
x <- rnbinom(200, mu = 5, size = 2)
pdistrs <- list("NB(5,0)"   = function (x) ppois(x, lambda=5),
                "NB(5,1/2)" = function (x) pnbinom(x, mu=5, size=2),
                "NB(5,1)"   = function (x) pnbinom(x, mu=5, size=1))
## reproduce Figure 1
op <- par(mfrow = c(1,3))
for (i in seq_along(pdistrs)) {
    pit(x, pdistrs[[i]], J = 10, relative = TRUE,
        plot = list(ylim=c(0,2.75), main = names(pdistrs)[i]))
    box()
}
par(op)
}

\keyword{dplot}

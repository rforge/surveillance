\name{checkResidualProcess}
\alias{checkResidualProcess}
\title{
Check the residual process of a fitted \code{twinSIR} or \code{twinstim}
}

\description{
  Transform the residual process (cf.
  \code{\link[=residuals.twinstim]{residuals}} method for classes
  \code{"twinSIR"} and \code{"twinstim"}) such that the transformed
  residuals should be uniformly distributed if the fitted model
  well describes the true conditional intensity function. Graphically
  check this using \code{\link{ks.plot.unif}}.

  The transformation for the residuals \code{tau} is
  \code{1 - exp(-diff(c(0,tau)))} (cf. Ogata, 1988).
}

\usage{
checkResidualProcess(object, plot = TRUE, ...)
}

\arguments{
  \item{object}{
    an object of class \code{"\link{twinSIR}"} or \code{"\link{twinstim}"}.
}
  \item{plot}{
    logical indicating if a plot of the transformed residuals should be
    produced by \code{\link{ks.plot.unif}} to check for deviations
    from the uniform distribution.
  }
  \item{\dots}{
    further arguments passed to \code{\link{ks.plot.unif}}.
  }
}

\value{
  A list (returned invisibly, if \code{plot = TRUE}) with the following
  components:
  \describe{
    \item{tau}{the residual process obtained by
      \code{residuals(object)}.}
    \item{U}{the transformed residuals which should be distributed as
      U(0,1).}
    \item{ks}{the result of the \code{ks.test} for the uniform
      distribution of \code{U}.}
    }
}

\references{
  Ogata, Y. (1988)
  Statistical models for earthquake occurrences and residual analysis
  for point processes.
  \emph{Journal of the American Statistical Association}, 83, 9-27
}

\author{
  Sebastian Meyer
}

\seealso{
  \code{\link{ks.plot.unif}} and the
  \code{\link[=residuals.twinstim]{residuals}}-method for classes
  \code{"twinSIR"} and \code{"twinstim"}.
}

\examples{
## TODO...
}

\keyword{dplot}
\keyword{htest}

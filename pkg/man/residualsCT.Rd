\name{residualsCT}
\alias{residuals.twinSIR}
\alias{residuals.twinstim}

\title{
Extract Cox-Snell-like residuals of a fitted point process
}

\description{
Extract the \dQuote{residual process} (cf. Ogata, 1988) of a fitted
point process modell specified through the conditional intensity
function, for instance a model of class \code{"\link{twinSIR}"} or
\code{"\link{twinstim}"}.
The residuals are defined as the fitted cumulative intensities at the
event times, and are generalized residuals similar to those discussed in
Cox and Snell (1968).
}

\usage{
\method{residuals}{twinSIR}(object, ...)
\method{residuals}{twinstim}(object, ...)
}

\arguments{
  \item{object}{
    an object of class \code{"\link{twinSIR}"} or \code{"\link{twinstim}"}.
  }
  \item{\dots}{unused (argument of the generic).}
}

\value{
Numeric vector of length the number of events of the corresponding point
process fitted by \code{object}. This is the observed residual process.
}

\references{
  Ogata, Y. (1988)
  Statistical models for earthquake occurrences and residual analysis
  for point processes.
  \emph{Journal of the American Statistical Association}, 83, 9-27

  Cox, D. R. & Snell, E. J. (1968)
  A general definition of residuals.
  \emph{Journal of the Royal Statistical Society. Series B (Methodological)}, 30, 248-275
}

\seealso{
  \code{\link{checkResidualProcess}} to graphically check the
  goodness-of-fit of the underlying model.
}

\author{
  Sebastian Meyer
}

\examples{
#TODO
}

\keyword{methods}

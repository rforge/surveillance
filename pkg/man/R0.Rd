\name{R0}
\alias{R0}
\alias{R0.twinstim}
\title{Computes basic reproduction numbers from fitted models}
\description{
The S3 generic function \code{R0} defined in package surveillance is intended to
compute basic reproduction numbers from fitted epidemic models.
The package currently defines a method for the \code{"\link{twinstim}"} class, which
computes mean numbers of infections caused by infected individuals depending on the event type
and marks attached to the individual, which contribute to the infection pressure
in the epidemic predictor of that class.
}
\usage{
R0(object, ...)

\method{R0}{twinstim}(object, newevents, dimyx = spatstat.options("npixel"), ...)
}
\arguments{
  \item{object}{A fitted epidemic model object for which an \code{R0} method exists.
  Currently, there is only a method for objects of class \code{"\link{twinstim}"}.}
  \item{newevents}{
a \code{data.frame} of events for which the basic reproduction number should be calculated. It must contain the factor variable \code{type}, the interaction ranges \code{eps.t} and \code{eps.s} and columns for the marks used in the epidemic component of the fitted twinstim object as stored in \code{formula(object)$epidemic}. The coding of the variables must of course be the same as used for fitting. For each row of \code{newevents} the corresponding R0 value is calculated. If \code{newevents} is omitted, then R0 values are returned for each type with all marks set to 0 and assuming the median interaction ranges from \code{object$medianeps}.
}
  \item{dimyx}{
	control parameter for the integration of the spatial interaction function by the two-dimensional midpoint rule (see \code{\link{polycub.midpoint}}). It is only relevant if \code{formula(object)$siaf$Fcircle} is undefined (\code{NULL}).
}
  \item{\dots}{currently unused (argument of the generic).}
}
\details{
For the \code{"\link{twinstim}"} class, the individual-specific mean number \eqn{\mu_j} of infections
caused by individual \eqn{j} inside its spatio-temporal range of interaction is defined as follows:
\deqn{%
\mu_j = e^{\eta_j} \cdot \int_0^\varepsilon g(t) dt \cdot \int_{b(\bold{0},\delta)} f(\bold{s}) d\bold{s}
}
where \eqn{b(\bold{0},\delta)} denotes the disc centred at (0,0)' with radius \eqn{\delta}, \eqn{\eta_j} is the epidemic linear predictor, \eqn{g(t)} is the temporal interaction function with interaction range \eqn{\varepsilon} (=\code{eps.t}), and \eqn{f(\bold{s})} is the spatial interaction function with interaction range \eqn{\delta} (=\code{eps.s}).
}
\value{
numeric vector of estimated basic reproduction numbers from the fitted model \code{object} corresponding to the rows of \code{newevents}.
}
\references{
  Meyer, S., Elias, J. and H\enc{ö}{oe}hle, M. (2011). A Space-Time Conditional
  Intensity Model for Invasive Meningococcal Disease Occurrence.
  Biometrics, in press. DOI: 10.1111/j.1541-0420.2011.01684.x
  A technical report is available as: http://epub.ub.uni-muenchen.de/11898/
}
\author{Sebastian Meyer}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

\keyword{methods}


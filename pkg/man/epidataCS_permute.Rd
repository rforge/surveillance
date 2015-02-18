\name{epidataCS_permute}
\alias{permute.epidataCS}
\title{
  Randomly Permute Time Points or Locations of \code{"epidataCS"}
}

\description{
  Monte Carlo tests for space-time interaction (\code{\link{epitest}})
  use the distribution of some test statistic under the null hypothesis
  of no space-time interaction. For this purpose, the function
  \code{permute.epidataCS} randomly permutes the time or space labels of
  the events.
}

\usage{
permute.epidataCS(x, what = c("time", "space", "both"),
                  keep, recreate = TRUE, verbose = FALSE)
}

\arguments{
  \item{x}{an object of class \code{"epidataCS"}.}
  \item{what}{character string determining what to permute: time points
    (default), locations or both.}
  \item{keep}{optional logical expression to be evaluated in the context
    of \code{x$events@data}, determining for which events the time and
    location should be kept as is. For instance, to keep some
    pre-history before time point 30 unchanged, use
    \code{keep = time <= 30}.}
  \item{recreate}{logical indicating if a new \code{"epidataCS"} object
    should be created (default) or if only the permuted events should be
    returned, i.e., a \code{"\linkS4class{SpatialPointsDataFrame}"}.}
  \item{verbose}{logical passed to \code{\link{as.epidataCS}} if
    \code{recreate = TRUE}.}
}

\value{
  depending on the \code{recreate} argument, either an
  \code{"epidataCS"} object (default) or a
  \code{"\linkS4class{SpatialPointsDataFrame}"}.
}

\author{
  Sebastian Meyer
}

\seealso{
  \code{\link{epitest}}
}

\examples{
data("imdepi")
imdepi_2y <- subset(imdepi, time < 2*365)  # for speed
set.seed(3)
permepi <- permute.epidataCS(imdepi_2y, what = "time", keep = time <= 30)
print(imdepi_2y, n = 8)
print(permepi, n = 8)
## the first 6 events are kept (as are all row.names),
## the time labels of the remaining events are shuffled
## (and events then again sorted by time),
## the marginal temporal distribution is unchanged
}

\keyword{manip}

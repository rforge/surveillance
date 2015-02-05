\name{knox}
\alias{knox}
\title{
Knox Test for Space-Time Interaction
}

\description{
Given temporal and spatial distances as well as corresponding critical
thresholds defining what \dQuote{close} means, the function
\code{knox} performs Knox (1963, 1964) test for space-time interaction.
The corresponding p-value can be calculated either by the Poisson
approximation or by a Monte Carlo permutation approach (Mantel, 1967).
}

\usage{
knox(dt, ds, eps.t, eps.s, simulate.p.value = FALSE, B = 999)
}

\arguments{
  \item{dt,ds}{
    numeric vectors of length \eqn{n*(n-1)/2} containing the pairwise
    temporal and spatial distances of \eqn{n} events.
    Logical vectors indicating temporal/spatial closeness may also be
    supplied, in which case \code{eps.t}/\code{eps.s} is ignored.
  }
  \item{eps.t,eps.s}{
    Critical distances defining closeness in time and space,
    respectively. Distances lower than or equal to the critical distance
    are considered \dQuote{"close"}.
  }
  \item{simulate.p.value}{
    logical indicating if a Monte Carlo permutation test should be
    performed instead of the default Poisson approximation. 
    See the note below.
  }
  \item{B}{
    number of permutations for the Monte Carlo approach.
  }
}

\note{
  The Poisson approximation works well if the proportions of close
  pairs in both time and space are small (Kulldorf and Hjalmars,
  1999), otherwise the Monte Carlo permutation approach is
  recommended.
}

\value{
  an object of class \code{"knox"} (inheriting from \code{"htest"}),
  which is a list with the following components:
  \item{statistic}{the number of close pairs.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
    performed, and whether the Poisson approximation or Monte Carlo
    simulation was used.}
  \item{data.name}{a character string giving the supplied \code{dt} and
    \code{ds} arguments.}
  \item{table}{the contingency table of \code{dt <= eps.t} and
    \code{ds <= eps.s}.}
}

\references{
  Knox, G. (1963):
  Detection of low intensity epidemicity: application to cleft lip and palate.
  \emph{British Journal of Preventive & Social Medicine}, \bold{17}, 121-127.
  
  Knox, E. G. (1964):
  The detection of space-time interactions.
  \emph{Journal of the Royal Statistical Society. Series C (Applied
    Statistics)}, \bold{13}, 25-30.

  Kulldorff, M. and Hjalmars, U. (1999):
  The Knox method and other tests for space-time interaction.
  \emph{Biometrics}, \bold{55}, 544-552.
  
  Mantel, N. (1967):
  The detection of disease clustering and a generalized regression approach.
  \emph{Cancer Research}, \bold{27}, 209-220.
}

\author{
  Sebastian Meyer
}

\seealso{
  the related \code{\link[ecodist]{mantel}} test (Mantel, 1967), which
  is implemented in package \pkg{ecodist}
}

\examples{
data("imdepi")
knox(dt = dist(imdepi$events$time), eps.t = 30,
     ds = dist(coordinates(imdepi$events)), eps.s = 50)
}

\keyword{htest}

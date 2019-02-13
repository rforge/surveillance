\name{surveillance-defunct}
\alias{surveillance-defunct}
\title{Defunct Functions in Package \pkg{surveillance}}
\alias{makePlot}
\alias{testSim}
\description{
  The functions listed here are no longer part of \pkg{surveillance}.
}
\usage{
## Defunct in surveillance 1.17.0
makePlot(outputpath, data = "k1", method = "rki1",
         name, disease, range = 157:339)
testSim(p = 0.99, r = 0.01, length = 400, A = 1, alpha = 1,
        beta = 0, phi = 0, frequency = 1, state = NULL, K, 
        range = 200:400)
}
\details{
  The ancient test function \code{makePlot} is no longer used and
  \code{testSim} is provided as an example
  in \code{help("\link{sim.pointSource}")}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}

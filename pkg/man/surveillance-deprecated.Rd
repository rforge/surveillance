\name{surveillance-deprecated}
\title{Deprecated Functions in Package \pkg{surveillance}}
\alias{surveillance-deprecated}
\alias{qlomax}
\alias{readData}
\description{
  The functions listed here are provided for compatibility
  with older versions of \pkg{surveillance} only,
  and may be defunct as soon as of the next release.
}
\usage{
qlomax(p, scale, shape)

readData(abb, week53to52=TRUE, sysPath=TRUE)
}
\arguments{
  \item{p}{vector of probabilities.}
  \item{scale}{positive scale parameter.}
  \item{shape}{positive shape parameter.}
  
  \item{abb}{abbreviation of the diseasename.}
  \item{week53to52}{Boolean indicating whether to convert RKI 53 Weeks System to 52 weeks a year.}
  \item{sysPath}{Boolean, if \code{TRUE} then R automatically looks in
    the extdata directory of the \pkg{surveillance} installation.}
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}

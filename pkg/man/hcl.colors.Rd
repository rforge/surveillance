\name{hcl.colors}
\alias{hcl.colors}
\title{
  HCL-based Heat Colors from the \pkg{colorspace} Package
}
\description{
  If package \pkg{colorspace} is available, its
  \link[colorspace]{heat_hcl} function is used to generate a color
  palette. Otherwise, the function uses the Heat 2 palette from
  R's own \code{hcl.colors} (in R >= 3.6.0) or falls back to
  basic \code{\link{heat.colors}}.
}
\usage{
hcl.colors(ncolors = 100, use.color = TRUE)
}
\arguments{
  \item{ncolors}{the number of colors (>= 1) to be in the palette.}
  \item{use.color}{logical. Should the palette use colors?
    Otherwise grey levels are returned.}
}
\value{
  A character vector of \code{ncolors} colors.
}

\examples{
barplot(rep(1,10), col = hcl.colors(10), axes = FALSE)
}

\keyword{color}
\keyword{dplot}

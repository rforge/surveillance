\name{sts_ggplot}
\alias{autoplot.sts}
\title{
  Time-Series Plots for \code{"sts"} Objects Using \pkg{ggplot2}
}
\description{
  A simple \CRANpkg{ggplot2} variant of \code{\link{stsplot_time}},
  based on a \dQuote{tidy} version of the \code{"sts"} object via
  \code{\link{tidy.sts}}.
}
\usage{
autoplot.sts(object, units = NULL, as.one = FALSE, scales = "fixed", ...)
}
\arguments{
  \item{object}{an object of class \code{"\linkS4class{sts}"}.}
  \item{units}{optional integer or character vector to select the units
    (=columns of \code{object}) to plot. The default (\code{NULL}) is
    to plot all time series.}
  \item{as.one}{logical indicating if all time series should be plotted
    in one panel with \code{\link[ggplot2]{geom_line}}.
    By default, the time series are plotted in separate panels (using
    \code{\link[ggplot2]{geom_bar}}).}
  \item{scales}{passed to \code{\link[ggplot2]{facet_wrap}}
    (for \code{as.one=FALSE}). By default, all panels use a common
    \code{ylim} (and \code{xlim}).}
  \item{\dots}{unused (argument of the generic).}
}
\value{
  a \code{"ggplot"} object.
}
\author{
  Sebastian Meyer
}

\seealso{
  \code{\link{stsplot_time}} for the traditional plots.
}

\examples{
if (requireNamespace("ggplot2")) {
    data("measlesDE")
    plot(measlesDE)
    autoplot.sts(measlesDE)
}
}

\keyword{hplot}
\keyword{ts}

\name{layout.labels}
\alias{layout.labels}
\alias{layout.scalebar}

\title{
Layout Items for \code{spplot}
}

\description{
Generate \code{sp.layout} items for use in \code{\link{spplot}}.
Function \code{layout.labels} draws labels at the coordinates of the
spatial object, and \code{layout.scalebar} returns a labeled scale bar.
}

\usage{
layout.labels(obj, labels = TRUE)

layout.scalebar(obj, corner = c(0.05, 0.95), scale = 1,
                labels = c(0, scale), height = 0.05)
}

\arguments{
  \item{obj}{
    an object inheriting from a \code{\linkS4class{Spatial}} class.
  }
  \item{labels}{
    specification of the labels. For \code{layout.labels}:
    \itemize{
      \item a \code{FALSE} or \code{NULL} value omits labels
      (\code{NULL} is returned),
      \item \code{labels = TRUE} uses \code{row.names(obj)},
      \item a character or numeric index for a column of \code{obj@data}
      which contains suitable labels,
      \item a vector of length \code{length(obj)} with labels,
      \item or a list of arguments for \code{\link[lattice]{panel.text}},
      where the optional \code{labels} component follows the same rules
      as above.
    }
    For \code{layout.scalebar}, a character vector of length two giving
    the labels to be put above the left and right ends of the scale bar.
  }
  \item{corner}{the location of the scale bar in the unit square, where
    \code{c(0,0)} refers to the bottom left corner. By default, the
    scale bar is placed in the top left corner (with a small buffer).}
  \item{scale}{the width of the scale bar.
    Take the \code{\link{proj4string}(obj)} into account.
    Note that a proper scale bar requires projected coordinates.}
  \item{height}{the height of the scale bar, see
    \code{\link{layout.scale.bar}}.}
}

\value{
  For \code{layout.labels}, a single \code{sp.layout} item, which is
  a list with first element \code{"panel.text"} and subsequent elements
  being arguments to that function based on the \code{labels}
  specification.

  For \code{layout.scalebar}, a list of \code{sp.layout} items
  comprising the polygonal scale bar and the labels.
}

\author{
Sebastian Meyer
}

\examples{
## labels function
data("measlesWeserEms")
(li <- layout.labels(measlesWeserEms@map, labels = list(font=2, labels="GEN")))
spplot(measlesWeserEms@map, zcol = "AREA", sp.layout = list(li),
       col.regions = rev(heat.colors(100)))

## scalebar function
load(system.file("shapes", "districtsD.RData", package = "surveillance"))
bavaria <- districtsD[substr(row.names(districtsD), 1, 2) == "09", ]
sb <- layout.scalebar(bavaria, corner = c(0.75,0.9), scale = 50,
                      labels = c("0", "50 km"))
spplot(bavaria, zcol = "POPULATION", sp.layout = sb,
       col.regions = rev(heat.colors(100)))
}

\keyword{aplot}
\keyword{dplot}

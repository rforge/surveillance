\name{glm_epidataCS}
\alias{glm_epidataCS}
\title{
  Fit an Endemic-Only \code{twinstim} as a Poisson-\code{glm}
}

\description{
  An endemic-only \code{\link{twinstim}} is equivalent to a Poisson
  regression model for the aggregated number of events,
  \eqn{Y_{[t][\bm{s}],k}}, by time-space-type cell. The rate of the
  corresponding Poisson distribution is
  \eqn{e_{[t][\bm{s}]} \cdot \lambda([t],[\bm{s}],k)},
  where \eqn{e_{[t][\bm{s}]} = |[t]| |[\bm{s}]|} is a multiplicative
  offset. Thus, the \code{\link{glm}} function can be used to fit
  an endemic-only \code{twinstim}, however, it is usually slower.
}

\usage{
glm_epidataCS(formula, data, ...)
}

\arguments{
  \item{formula}{
    an endemic model formula without response, comprising variables of
    \code{data$stgrid} and possibly the variable \code{type} for a
    type-specific model.
  }
  \item{data}{
    an object of class \code{"\link{epidataCS}"}.
  }
  \item{\dots}{
    arguments passed to \code{\link{glm}}. Note that \code{family} and
    \code{offset} are fixed internally.
  }
}

\value{
  a \code{\link{glm}}
}

\author{
  Sebastian Meyer
}

\examples{
data("imdepi")
data("imdepifit")
## Compare an endemic-only twinstim() with an equivalent model using glm()
fit_twinstim <- update(imdepifit, epidemic = ~0, siaf = NULL, cumCIF=FALSE)
fit_glm <- glm_epidataCS(formula(fit_twinstim)$endemic, imdepi)
coef(fit_glm)
\dontshow{
stopifnot(isTRUE(all.equal(coef(fit_glm), coef(fit_twinstim),
                           tolerance = 0.0005, check.attributes = FALSE)))
## also check type-specific model:
stopifnot(isTRUE(all.equal(
    coef(glm_epidataCS(~0+type, imdepi)),
    coef(update(fit_twinstim, endemic=~(1|type), verbose=FALSE,
                optim.args=list(control=list(trace=0)))),
    tolerance = 0.0005, check.attributes = FALSE)))
}
}

\keyword{models}

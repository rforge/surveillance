\name{calibrationTest}
\alias{calibrationTest}
\alias{dss}
\alias{logs}
\alias{rps}

\title{
  Calibration Test for Poisson or Negative Binomial Predictions
}

\description{
  The implemented calibration tests for Poisson or negative binomial
  predictions of count data are based on proper scoring rules and
  described in detail in Wei and Held (2014).
  The following proper scoring rules are available:
  Dawid-Sebastiani score (\code{"dss"}),
  logarithmic score (\code{"logs"}),
  ranked probability score (\code{"rps"}).
}

\usage{
calibrationTest(x, mu, size = NULL,
                which = c("dss", "logs", "rps"), method = 2)

dss(x, mu, size = NULL)
logs(x, mu, size = NULL)
rps(x, mu, size = NULL, k = 40, tolerance = sqrt(.Machine$double.eps))
}

\arguments{
  \item{x}{
    numeric vector of observed counts. All involved functions are
    vectorized and also accept matrices or arrays.
    The score functions preserve the dimensions.
  }
  \item{mu}{
    numeric vector of means of the predictive distributions for the
    observations \code{x}.
  }
  \item{size}{
    either \code{NULL} (indicating Poisson predictions with mean
    \code{mu}) or a numeric vector of dispersion parameters of the
    negative binomial distributions for the observations \code{x}.
  }
  \item{which}{
    character string indicating which proper scoring rule to apply.
  }
  \item{method}{
    selection of the \eqn{z}-statistic: \code{method = 2} refers to
    the alternative test statistic \eqn{Z_s^*} of Wei and Held (2014,
    Discussion), which has been recommended for low counts.
    \code{method = 1} corresponds to Equation 5 in Wei and Held (2014).
  }
  \item{k,tolerance}{
    arguments controlling the finite sum approximation for the RPS
    with truncation at \code{ceiling(mu + k*sd)}.
  }
}

\value{
  an object of class \code{"htest"},
  which is a list with the following components:
  \item{method}{a character string indicating the type of test
    performed (including \code{which} scoring rule).}
  \item{data.name}{a character string naming the supplied \code{x} argument.}
  \item{statistic}{the \eqn{z}-statistic of the test.}
  \item{parameter}{the number of predictions underlying the test, i.e., \code{length(x)}.}
  \item{p.value}{the p-value for the test.}
}

\references{
  Wei, W. and Held, L. (2014):
  Calibration tests for count data.
  \emph{Test}, \bold{23}, 787-805.  
}

\author{
  Sebastian Meyer, Michaela Paul, and Wei Wei
}

\examples{
## a univariate surveillance time series
data("salmonella.agona")
salmonella <- disProg2sts(salmonella.agona)

## a hhh4() model
model <- list(end = list(f = addSeason2formula(~1 + t)),
              ar = list(f = ~1),
              family = "NegBin1")
fit <- hhh4(salmonella, model)

## sequential one-step-ahead predictions for the last 5 weeks
pred <- oneStepAhead(fit, nrow(salmonella)-5, type="rolling",
                     which.start="final", verbose=FALSE)
pred

## calibration test
with(pred, calibrationTest(x = observed, mu = pred, size = exp(psi)))
}

\keyword{htest}

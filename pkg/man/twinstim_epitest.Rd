\name{twinstim_epitest}
\alias{epitest}
\alias{plot.epitest}
\title{Permutation Test for Space-Time Interaction in \code{"twinstim"}}

\description{
  The function \code{epitest} takes a simple epidemic \code{"twinstim"}
  model (one with \code{epidemic = ~1}) and tests if the spatio-temporal
  interaction invoked by the \code{epidemic} model component is
  statistically significant.
  The test statistic is the reproduction number \code{\link{simpleR0}}.
  %% A likelihood ratio test of the supplied epidemic model against
  %% the corresponding endemic-only model is performed, i.e., the test
  %% statistic is twice the log-likelihood difference of the models.
  Its null distribution is obtained by a Monte Carlo permutation
  approach (via \code{\link{permute.epidataCS}}) similar to the
  \code{\link{knox}} test.
  There is a simple \code{plot}-method showing a \code{\link{truehist}} of
  the simulated null distribution together with the observed value.
}

\usage{
epitest(model, data, B = 199, eps.s = NULL, eps.t = NULL,
        fixed = NULL, verbose = TRUE, ...)

\method{plot}{epitest}(x, ...)
}

\arguments{
  \item{model}{
    a simple epidemic \code{"\link{twinstim}"} without covariates, i.e.,
    \code{epidemic = ~1}. This is because covariate effects in the
    epidemic component are not well identified when there is no
    space-time interaction such as in the permuted data. Estimating a
    rich epidemic \code{model} under the null hypothesis of no
    space-time interaction will most likely result in singular convergence.
  }
  \item{data}{
    an object of class \code{"\link{epidataCS}"}, the \code{data} to
    which the \code{model} was fitted.
  }
  \item{B}{
    the number of permutations for the Monte Carlo approach.
    The default number is rather low; if computationally feasible,
    \code{B = 999} is more appropriate. Note that this determines the
    \dQuote{resolution} of the p-value: the smallest attainable p-value
    is \code{1/(B+1)}.
  }
  \item{eps.s,eps.t}{arguments for \code{\link{simpleR0}}.}
  \item{fixed}{
    optional character vector naming parameters to fix at their original
    value when re-fitting the \code{model} on permuted data.
  }
  \item{verbose}{
    the amount of tracing in the range \code{0:2}.
    Level 0 (or \code{FALSE}) means no output,
    level 1 (or \code{TRUE}, the default) will show a progress bar,
    and level 2 prints the test statistic resulting from each permutation.
    Does not work if parallelized using clusters.
    See \code{\link{plapply}} for other choices.
  }
  \item{\dots}{further arguments for \code{\link{plapply}} to configure
    parallel operation, i.e., \code{.parallel} as well as
    \code{.seed} to make the results reproducible.\cr
    For the \code{plot}-method, further arguments passed to
    \code{\link{truehist}}.
  }
  \item{x}{
    an object of class \code{"epitest"} as returned by \code{epitest}.
  }
}

\value{
  a list (inheriting from \code{"htest"}) with the following components:
  \item{method}{a character string indicating the type of test performed.}
  \item{data.name}{a character string giving the supplied \code{data} and
    \code{model} arguments.}
  \item{statistic}{the observed test statistic.}
  \item{p.value}{the p-value for the test.
    It is based on the subset of convergent fits only.}
  \item{permfits}{the list of model fits (endemic-only and epidemic)
    from the \code{B} permutations.}
  \item{permstats}{a data frame with \code{B} rows and the columns
    \code{"l0"} (log-likelihood of the endemic-only model \code{m0}),
    \code{"l1"} (log-likelihood of the epidemic model \code{m1}),
    \code{"D"} (twice their difference),
    \code{"simpleR0"} (the results of \code{\link{simpleR0}(m1, eps.s, eps.t)}), 
    and \code{"converged"} (a boolean indicator if both models converged).}

  The \code{plot}-method invisibly returns \code{NULL}.
}

\author{
  Sebastian Meyer
}

\seealso{
  \code{\link{permute.epidataCS}}, \code{\link{knox}}
}

\examples{
data("imdepi")
data("imdepifit")

## for the example to run quickly, we only use B-cases,
## assume spatial interaction to be constant within 50 km,
## and compute only a few permutations (in parallel)
imdepiB50 <- update(subset(imdepi, type == "B"), eps.s = 50)
imdfitB50 <- update(imdepifit, data = imdepiB50,
                    epidemic = ~1, siaf = NULL, control.siaf = NULL)
et <- epitest(imdfitB50, imdepiB50, B = 9, verbose = 2,
              .seed = 1, .parallel = 2)
et
plot(et)
}

\keyword{htest}

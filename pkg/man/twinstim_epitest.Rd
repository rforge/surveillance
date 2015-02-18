\name{twinstim_epitest}
\alias{epitest}
\title{Permutation Test for Space-Time Interaction in \code{"twinstim"}}

\description{
  The function \code{epitest} takes an epidemic \code{"twinstim"} model
  and tests if the spatio-temporal interaction invoked by the
  \code{epidemic} model component is statistically significant.
  A likelihood ratio test of the supplied epidemic model against
  the corresponding endemic-only model is performed, i.e., the test
  statistic is twice the log-likelihood difference of the models.
  Its null distribution is obtained by a Monte Carlo permutation
  approach (via \code{\link{permute.epidataCS}}) similar to the
  \code{\link{knox}} test.
}

\usage{
epitest(model, data, B = 199, seed = NULL, cores = 1, verbose = TRUE)
}

\arguments{
  \item{model}{
    an object of class \code{"\link{twinstim}"} with an
    epidemic model component, usually \code{epidemic = ~1}
    without covariates. This is because covariate effects in the
    epidemic component are not well identified when there is no
    space-time interaction such as in the permuted data. Estimating a
    rich epidemic \code{model} under the null hypothesis of no
    space-time interaction will most likely result in singular convergence.
  }
  \item{data}{
    an object of class \code{"\link{epidataCS}"}, the data to which the
    \code{model} was fitted.
  }
  \item{B}{
    the number of permutations for the Monte Carlo approach.
    The default number is rather low; if computationally feasible,
    \code{B = 999} is more appropriate. Note that this determines the
    \dQuote{resolution} of the p-value: the smallest attainable p-value
    is \code{1/(B+1)}.
  }
  \item{seed}{
    an object specifying how the random number generator should be
    initialized (via \code{\link{set.seed}}). By default (\code{NULL}),
    no care of the \code{\link{.Random.seed}} is taken. If
    non-\code{NULL}, the original \code{.Random.seed} will be restored
    afterwards. If \code{cores > 1}, the \code{\link{RNGkind}}
    \code{"L'Ecuyer-CMRG"} will be used for reproducibility, see the
    description of \code{\link[parallel]{mcparallel}} in package
    \pkg{parallel}.
  }
  \item{cores}{
    number of processes to use in parallel operation using forking
    (which is not available on Windows, see
    \code{\link[parallel]{mclapply}} in package \pkg{parallel}).
  }
  \item{verbose}{
    the amount of tracing in the range \code{0:2}.
    Level 0 (or \code{FALSE}) means no output,
    level 1 (or \code{TRUE}, the default) will show a
    \code{\link{txtProgressBar}} (if \code{cores = 1} in an
    \code{\link{interactive}} \R session) or output a dot (otherwise)
    after each permutation,
    and level 2 means to print the permutation test statistics as they
    arrive.
    Other choices for the dot in level 1 are possible by
    specifying the desired symbol as the \code{verbose} argument.
  }
}

\value{
  a list (inheriting from \code{"htest"}) with the following components:
  \item{method}{a character string indicating the type of test performed.}
  \item{data.name}{a character string giving the supplied \code{data} and
    \code{model} arguments.}
  \item{statistic}{the observed likelihood ratio test statistic.}
  \item{p.value}{the p-value for the test. It is based on the subset of
    convergent fits only. Note that the test statistic does not
    have the usual asymptotic chi square distribution (which is why we
    do the permutation test), but the corresponding p-value is still
    attached as an attribute \code{"chisq"} for comparison.}
  \item{permfits}{the list of model fits (endemic-only and epidemic)
    from the \code{B} permutations.}
  \item{permstats}{a data frame with \code{B} rows and the columns
    \code{"l0"} (log-likelihood of the endemic-only model),
    \code{"l1"} (log-likelihood of the epidemic model),
    \code{"D"} (twice their difference), and
    \code{"converged"} (a boolean indicator if both models converged).}
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

## for the example to run quickly,
## we use the cases of the last two years only,
## assume spatial interaction to be constant within 20 km,
## and compute only a few permutations (in parallel on Unix-alikes)
imdepi2 <- update(subset(imdepi, time > 1826), eps.s = 20)
imdfit2 <- update(imdepifit, data = imdepi2, t0 = 1826,
                  epidemic = ~1, siaf = NULL, control.siaf = NULL)
mc.cores <- if(.Platform$OS.type == "unix") getOption("mc.cores", 2) else 1
et <- epitest(imdfit2, imdepi2, B = 19, seed = 1, cores = mc.cores)
et

## histogram of the permutation distribution
hist(et$permstats$D, xlab = "Test statistic",
     main = "Permutation test for space-time interaction")
abline(v = et$statistic, lwd = 2)
axis(3, at = et$statistic, labels = "Data",
     tick = FALSE, line = -1, font = 2)
}

\keyword{htest}

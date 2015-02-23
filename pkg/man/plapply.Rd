\name{plapply}
\alias{plapply}
\title{Parallel \code{lapply}}

\description{
Parallelized version of \code{lapply} wrapping around
\code{\link[parallel]{mclapply}} and \code{\link[parallel]{parLapply}}
in the base package \pkg{parallel}.
This wrapper can take care of the \code{.Random.seed} and
print progress information (not for cluster-based parallelization).
With the default arguments it equals \code{lapply}
enriched by a progress bar.
}

\usage{
plapply(X, FUN, ...,
        .cores = 1, .cl = NA, .seed = NULL, .verbose = TRUE)
}

\arguments{
  \item{X,FUN,\dots}{see \code{\link{lapply}}.}
  \item{.cores}{
    the number of processes to use in parallel operation using forking
    via \code{\link[parallel]{mclapply}}.
    By default (\code{.cores = 1}), \code{\link{lapply}} is used.
    This argument is ignored if a cluster is set (see \code{.cl}).
  }
  \item{.cl}{
    a cluster object (see \code{\link[parallel]{makeCluster}}) or
    \code{NULL} (registered default cluster) for parallel computations
    using clusters via \code{\link[parallel]{parLapply}}.
    The default \code{NA} means to use \code{.cores} parallel processes
    via forking (which is recommended, but not supported on Windows).
  }
  \item{.seed}{
    If set (non-\code{NULL}), results involving random number generation
    become reproducible. If using a cluster (via the \code{.cl} argument),
    \code{\link[parallel]{clusterSetRNGStream}} is called with the
    specified \code{.seed} before running \code{parLapply}. Otherwise,
    \code{\link{set.seed}(.seed)} is run and the \code{\link{RNGkind}}
    is changed to \code{"L'Ecuyer-CMRG"} if \code{cores > 1} (see the
    description of \code{\link[parallel]{mcparallel}} in package
    \pkg{parallel}). The original \code{\link{.Random.seed}} will be
    restored \code{on.exit} of the function.
  }
  \item{.verbose}{
    if and how progress information should be displayed, i.e., what to
    do on each exit of \code{FUN}. This is unsupported and ignored for
    cluster-based parallelization using the \code{.cl} argument and
    primitive \code{FUN}ctions.
    The default (\code{TRUE}) will show a \code{\link{txtProgressBar}}
    (if \code{cores = 1} in an \code{\link{interactive}} \R session) or
    \code{cat(".")} (otherwise). Other choices for the dot are possible
    by specifying the desired symbol directly as the \code{.verbose}
    argument. Alternatively, \code{.verbose} may be any custom call or
    expression to be executed \code{\link{on.exit}} of \code{FUN} and
    may thus involve any objects from the local evaluation environment.
  }
}

\value{
  a list of the results of calling \code{FUN} on each value of \code{X}.
}

\author{
  Sebastian Meyer
}

\seealso{
  \code{\link[parallel]{mclapply}} and \code{\link[parallel]{parLapply}}
}

\examples{
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))

## if neither parallel nor verbose then this simply equals lapply()
plapply(x, quantile, probs = 1:3/4, .verbose = FALSE)

## verbose lapply() -- not really useful for such fast computations
res <- plapply(x, quantile, probs = 1:3/4, .verbose = TRUE)
res <- plapply(x, quantile, probs = 1:3/4, .verbose = "|")
res <- plapply(x, quantile, probs = 1:3/4,
               .verbose = quote(cat("length(x) =", length(x), "\n")))

## setting the seed for reproducibility of results involving the RNG
samp <- plapply(as.list(1:3), runif, .seed = 1)

## parallel lapply()
library("parallel")
## using forking (available on Unix only)
if (.Platform$OS.type == "unix") {
    res <- plapply(x, quantile, probs = 1:3/4, .cores = 2)
}
## using clusters (especially for Windows)
cl <- makeCluster(getOption("cl.cores", 2))
res <- plapply(x, quantile, probs = 1:3/4, .cl = cl)
stopCluster(cl)
}

\keyword{iteration}
\keyword{list}

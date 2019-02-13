\name{surveillance-defunct}
\alias{surveillance-defunct}
\title{Defunct Functions in Package \pkg{surveillance}}
\alias{compMatrix.writeTable}
\alias{makePlot}
\alias{test}
\alias{testSim}
\alias{toFileDisProg}
\description{
  The functions listed here are no longer part of \pkg{surveillance}.
}
\usage{
## Defunct in surveillance 1.17.0
compMatrix.writeTable(compMatrix)
makePlot(outputpath, data = "k1", method = "rki1",
         name, disease, range = 157:339)
test(data = c("k1", "m5"), range = 157:339)
testSim(p = 0.99, r = 0.01, length = 400, A = 1, alpha = 1,
        beta = 0, phi = 0, frequency = 1, state = NULL, K, 
        range = 200:400)
toFileDisProg(disProgObj, toFile)
}
\details{
  The trivial function \code{compMatrix.writeTable} is no longer used
  (it did nothing more than generating an \code{\link{xtable}} of its input).
  
  The ancient test function \code{makePlot} is no longer used,
  \code{test} calls of outbreak detection algorithms applied to the old
  SurvStat datasets can be found in \code{vignette("surveillance")},
  and \code{testSim} is provided as an example
  in \code{help("\link{sim.pointSource}")}.

  Functions related to the old \code{"\link[=create.disProg]{disProg}"}
  class are no longer needed. The effect of \code{toFileDisProg} could
  still be achieved via \code{\link{write.table}} of
  \code{as.data.frame(disProg2sts(disProgObj))[c("epoch", "observed", "state")]}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}

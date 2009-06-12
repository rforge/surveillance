\name{algo.farrington.threshold}
\alias{algo.farrington.threshold}
\encoding{latin1}

\title{Threshold computations using a two sided confidence interval}
\description{
Depending on the current transformation \eqn{h(y)= \{y, \sqrt{y}, y^{2/3}\}},

  \deqn{V(h(y_0)-h(\mu_0))=V(h(y_0))+V(h(\mu_0))}

  is used to compute a prediction interval. The prediction variance
  consists of a component due to the variance of having a single
  observation and a prediction variance.  }

\usage{
algo.farrington.threshold(pred,phi,alpha=0.01,skewness.transform="none")
}
\arguments{
\item{pred}{A GLM prediction object}
\item{phi}{Current overdispersion (superflous?)}
\item{alpha}{Quantile level in Gaussian based CI, i.e. an \eqn{(1-\alpha)\%}
    confidence interval is computed. }
\item{skewness.transform}{Skewness correction, i.e. one of
    \code{"none"}, \code{"1/2"}, or \code{"2/3"}.}
\item{y}{Observed number}
}
\value{
  Vector of length 4 with lower and upper bounds of an \eqn{(1-\alpha)\%} confidence 
  interval (first two arguments) and corresponding quantile of observation \code{y} 
  together with the median of the predictive distribution.
}
\keyword{regression}


\begin{Schunk}
\begin{Sinput}
> refvalIdxByDate <- function(t0, b, w, epochStr, epochs) {
+     refDays <- NULL
+     refPoints <- seq(t0, length = b + 1, by = "-1 year")[-1]
+     for (j in 1:length(refPoints)) {
+         refPointWindow <- c(rev(seq(refPoints[j], length = w + 
+             1, by = paste("-", epochStr, sep = ""))), seq(refPoints[j], 
+             length = w + 1, by = epochStr)[-1])
+         refDays <- append(refDays, refPointWindow)
+     }
+     if (epochStr == "1 week") {
+         refDays <- refDays - (as.numeric(format(refDays, "%w")) - 
+             1)
+     }
+     if (epochStr == "1 month") {
+         refDays <- refDays - (as.numeric(format(refDays, "%d")) - 
+             1)
+     }
+     wtime <- match(as.numeric(refDays), epochs)
+     return(wtime)
+ }
\end{Sinput}
\end{Schunk}


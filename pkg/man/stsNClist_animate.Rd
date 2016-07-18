\name{stsNClist_animate}
\alias{stsNClist_animate}
\encoding{latin1}

\title{Animate a sequence of nowcasts}
\description{Animate a sequence of nowcasts stored as a list.
}
\usage{
animate_nowcasts(nowcasts,linelist_truth, method="bayes.trunc.ddcp",
                               control=list(dRange=NULL,anim.dRange=NULL, plot.dRange=NULL,consistent=FALSE,ylim=NULL,cex.names=0.7,col=c("violetred3","#2171B5","orange","blue","black","greenyellow")),showLambda=TRUE)
}
\arguments{
  \item{nowcasts}{A list of objects of class \code{stsNC}}
  \item{linelist_truth}{True linelist}
  \item{method}{Which method to show (has to be present in the
    nowcasts)}
  \item{control}{List with control options}
}
\value{
  This function is experimental and is not yet documented.
}
\details{
}
\author{M. \enc{Höhle}{Hoehle}}
\examples{
}
\keyword{hplot}














% magic.dim

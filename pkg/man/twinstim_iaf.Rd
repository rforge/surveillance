%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rd file for space and time interactions functions for 'twinstim'
%% fitting.
%%
%% Author: Michael Höhle based on the in-code documentation of
%%         Sebastian Meyer.
%% $Date: 2010-07-07 00:12:46 +0200 (Wed, 07 Jul 2010) $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\encoding{latin1}
\name{iaf}
\alias{siaf.gaussian}
\alias{siaf.constant}
\alias{tiaf.exponential}
\alias{tiaf.constant}
%Not documented -- internal functions
\alias{.checkiaf0}
\alias{.checknargs3}
\alias{checksiaf}
\alias{checktiaf}
\alias{.parseiaf}

\title{
  Temporal and spatial interaction function for \code{twinstim} fitting.
}
\description{
  Predefined functional forms for the temporal- and spatio interaction
  function (f and g, respectively) for use in the twinstim model as
  described in Meyer et al. (2010). 
}
\usage{
siaf.gaussian(nTypes, logsd = TRUE, density = FALSE, effRangeMult = 6, validpars = NULL)
siaf.constant()
tiaf.exponential(nTypes)
tiaf.constant()
}
\arguments{
  \item{nTypes}{Number of different types, i.e. \eqn{|\mathcal{K}|}{|K|}}
  \item{logsd}{Parametrize standard deviation parameter
  \eqn{\sigma>0}{sigma>0} on log scale to enforce parameter
  constraint. Default: \code{TRUE}}
  \item{density}{Normalize expression (\code{TRUE}) or just use Kernel
  of the density (\code{FALSE}).}
  \item{effRangeMult}{}
  \item{validpars}{}
}
\references{
  S. Meyer, J. Elias and M. Höhle (2010): A space-time conditional
  intensity model for infectious disease occurence, Department of
  Statistics: Technical Reports, No. 95, 27 pages, 2010.
  Available as http://epub.ub.uni-muenchen.de/11898/
  
  Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
  on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet}
  München. Available as http://epub.ub.uni-muenchen.de/11703/
}
\author{
  S. Meyer with documentation by M. \enc{Höhle}{Hoehle}.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{surveillance::twinstim}}.
}
\examples{
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


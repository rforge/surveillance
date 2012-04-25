%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rd file for space and time interactions functions for 'twinstim'
%% fitting.
%%
%% Author: Michael Höhle based on the in-code documentation of
%%         Sebastian Meyer.
%% $Date: 2010-07-07 00:12:46 +0200 (Wed, 07 Jul 2010) $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\encoding{latin1}
\name{twinstim_iaf}
\alias{siaf.gaussian}
\alias{siaf.constant}
\alias{tiaf.exponential}
\alias{tiaf.constant}

\title{
  Temporal and spatial interaction function for \code{twinstim} fitting
}
\description{
  Predefined functional forms for the temporal and spatial interaction
  functions (f and g, respectively) for use in the \code{twinstim} model as
  described in Meyer et al. (2011). 
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
  \item{effRangeMult}{Determines the effective range for numerical integration
  in terms of multiples of \eqn{\sigma}, i.e. with \code{effRangeMult=6} numerical
  integration only considers the 6-sigma area around the event instead of the whole
  observation region W.}
  \item{validpars}{See \code{\link{twinstim}}. If \code{logsd = FALSE}, you should
  either use constrained optimisation (L-BFGS-B) or set \code{validpars} to
  \code{function (pars) pars > 0}.}
}
\value{
A list containing the interaction function, its derivative, ...
%TODO
}
\references{
  A space-time conditional intensity model for invasive meningococcal
  disease occurrence (2011), S. Meyer, J. Elias and M. H\enc{ö}{oe}hle,
  Biometrics, Epub ahead of print, DOI: 10.1111/j.1541-0420.2011.01684.x.
  A technical report is available as: http://epub.ub.uni-muenchen.de/11898/
  
  Meyer, S. (2010): Spatio-Temporal Infectious Disease Epidemiology based
  on Point Processes. Master Thesis, Ludwig-Maximilians-\enc{Universität}{Universitaet}
  München. Available as http://epub.ub.uni-muenchen.de/11703/
}
\author{
  S. Meyer with documentation by M. \enc{Höhle}{Hoehle}.
}
\seealso{
\code{\link{twinstim}}.
}
\examples{
#Finetype indenepdent spatial interaction function used in the twinstim paper
siaf_log1 <- siaf.gaussian(1, logsd = TRUE, density = FALSE, effRangeMult = 6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilities}


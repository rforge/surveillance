%This is just a way to "hide" functions, which the user should not know
%the proper way is to use NAMESPACE here (TODO!)

\name{surveillance-internal}

\alias{anscombeNB}
\alias{hypgeom2F1special}
\alias{hypGeomSmall}

%Some S4 stuff from sts
\alias{add.legend}
\alias{fix.dimnames}
\alias{hcl.colors}
\alias{insert.zeroes}
\alias{wait}
\alias{init.sts}
\alias{init.stsBP}
\alias{merge.list}
\alias{addFormattedXAxis}

%Functions from M.Paul
\alias{commonDenom}
\alias{logInterpolation}
\alias{getH}
\alias{getHK}
\alias{getTheta1}

%additional (undocumented) functions needed for algo.hhh
\alias{unpackParams}
\alias{gradient}
\alias{weightedSumNeighbours}
\alias{jacobian}
\alias{sinCos2amplitudeShift}
\alias{jacobianAmplitudeShift}
\alias{getLambda}
\alias{expAlpha.mm}
\alias{logLik.ah}
\alias{logLik.ahg}

%twins function not documented
\alias{make.nu}
\alias{make.pois}
\alias{pois.plot}
\alias{tms.plot}

%Utility functions
\alias{formatDate}
\alias{na2FALSE}
\alias{convertSTS}
\alias{doIt}

%hhh4 functions which are not documented
%\alias{permutationTest}
%\alias{scores}
%\alias{pit}
\alias{pit.one}
\alias{dss}
\alias{fun}
\alias{logScore}
\alias{nses}
\alias{permutationTest}
\alias{rps}
\alias{rps.one}
\alias{ses}

 
\title{Internal surveillance Functions}
\description{
  Undocumented internal \pkg{surveillance} functions. If you really
  want to know what's going on you might consider looking at the source
  code.
}
\details{
  These are not to be called by the user.
}
\keyword{ internal }

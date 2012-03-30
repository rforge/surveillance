%This is just a way to "hide" functions, which the user should not know
%the proper way is to use NAMESPACE here (TODO!)

\name{surveillance-internal}

% R CMD check also requires documentation for virtual S4 classes set by setOldClass()
\alias{owin-class}

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
%
%twins function not documented
\alias{make.nu}
\alias{make.pois}
\alias{pois.plot}
\alias{tms.plot}
%Utility functions
\alias{formatDate}
\alias{na2FALSE}

%Weird functions from sp package which produce warnings in R CMD check
%for the surveillance package (??)
\alias{summary,GridTopology-method}
\alias{summary,Spatial-method}

%twinSIR, twinstim and hhh4 functions which are not documented. This
%is more like a TODO list!!
\alias{Lambda}
\alias{Ztilde.chibarsq}
\alias{bdist}
\alias{checkEvents}
\alias{checkQ}
\alias{checkSimArgs}
\alias{checkstgrid}
\alias{coefficients.twinSIR}
\alias{convertSTS}
\alias{determineSources}
\alias{discpoly}
\alias{doIt}
\alias{dss}
\alias{fun}
\alias{gaussCub}
\alias{getSummary}
\alias{gridcellOfEvent}
\alias{head.epidataCS}
\alias{inside.gpc.poly}
\alias{intersectCircle}
\alias{isClosed}
\alias{likelihood.ci}
\alias{logScore}
\alias{marks.epidataCS}
\alias{multiplicity.sp}
\alias{nses}
\alias{obligColsNames_events}
\alias{obligColsNames_stgrid}
\alias{permutationTest}
\alias{pit}
\alias{pit.one}
\alias{polyCub.SV}
\alias{polyCub.midpoint}
\alias{print.simEpidataCSlist}
\alias{print.summary.twinSIR}
\alias{read.design}
\alias{read.model}
\alias{residuals.twinSIR}
\alias{residuals.twinstim}
\alias{rps}
\alias{rps.one}
\alias{scale.gpc.poly}
\alias{scores}
\alias{ses}
\alias{summary.epidataCS}
\alias{tail.epidataCS}
\alias{w.chibarsq.sim}

 
\title{Internal surveillance Functions}
\description{
  Undocumented internal \code{surveillance} functions. If you really
  want to know whats going on you might consider looking at the source
  code. If the package had a namespace you would not see this list. 
}
\details{
  These are not to be called by the user.
}
\keyword{ internal }

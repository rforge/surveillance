\encoding{latin1}
\name{hagelloch}
\alias{hagelloch}
\alias{hagelloch.df}
\docType{data}
\keyword{datasets}

\title{1861 Measles Epidemic in the City of Hagelloch, Germany}
\description{
  Data on the 188 cases in the measles outbreak among children in the
  German city of Hagelloch (near T\enc{ü}{ue}bingen) 1861. The data were
  originally collected by Dr. Albert Pfeilsticker (1863) and augmented and
  re-analysed by Dr. Heike Oesterle (1992).
}

\usage{
data("hagelloch")
}

\format{
  Loading \code{data("hagelloch")} gives two objects:
  \code{hagelloch} and \code{hagelloch.df}. The former is an
  \code{"\link{epidata}"} object for use with \code{\link{twinSIR}}
  containing the entire SIR event history of the outbreak in the
  population of 188 children.
  The latter is the original \code{data.frame} of 188 rows
  with individual information for each infected child. 

  The covariate information in either \code{hagelloch} or
  \code{hagelloch.df} are as follows:
  
  \describe{
    \item{PN:}{patient number}
    \item{NAME:}{patient name (as a factor)}
    \item{FN:}{family index}
    \item{HN:}{house number}
    \item{AGE:}{age in years}
    \item{SEX:}{gender of the individual (factor: male, female)}
    \item{PRO:}{\code{Date} of prodromes}
    \item{ERU:}{\code{Date} of rash}
    \item{CL:}{class (factor: preschool, 1st class, 2nd class)}
    \item{DEAD:}{\code{Date} of death (with missings)}
    \item{IFTO:}{number of patient who is the putative source of infection (0 = unknown)}
    \item{SI:}{serial interval = number of days between dates of prodromes of infection source and infected person}
    \item{C:}{complications (factor: no complicatons, bronchopneumonia, severe bronchitis, lobar pneumonia, pseudocroup, cerebral edema)}
    \item{PR:}{duration of prodromes in days}
    \item{CA:}{number of cases in family}
    \item{NI:}{number of initial cases}
    \item{GE:}{generation number of the case}
    \item{TD:}{day of max. fever (days after rush)}
    \item{TM:}{max. fever (degree celsius)}
    \item{x.loc:}{x coordinate of house (in meters). Scaling in metres
      is obtained by multiplying the original coordinates by 2.5 (see details
      in Neal and Roberts (2004))}
    \item{y.loc:}{y coordinate of house (in meters). See also the above
      description of \code{x.loc}.}
  }

  The \code{hagelloch} \code{"epidata"} object additionally contains the
  following columns:

  \describe{
    \item{tS:}{Time of symptoms (here: prodomes) in days after the start of the
      epidemic 30 Oct 1861. This corresponds to the time of the
    "Exposed"->"Infectious" transition in an SEIR model.}
    \item{tQ:}{Time upon which the rash first appears.}
    \item{tD:}{Time of death, if available, in day after start of the
      epidemic.} 
    \item{tI:}{Time at which individual becomes infected. This
      corresponds to the transition from state "Susceptible" to
      "Exposed" in an "SEIR" model. Actually
      this time is unknown, but we use
      \deqn{I_i = S_i - d_1,}{I[i] = S[i] - d[1],} where  
      \eqn{d_1=1}{d[1]=1} is the latency period. See Neal and Roberts
    (2004) for details.} 
    \item{tR:}{Time at which the infectious period of the individual ends,
      i.e. time at which the individual moves from "Infectious" to
      "Recovered". This unknown
      time is calculated as
      \deqn{R_i = \min{D_i,Q_i+d_0},}{R[i] = min(D[i],Q[i]+d[0]),}
      where -- as in Section 3.1 of Neal and Roberts (2004) -- we use
      \eqn{d_0=3}{d0=3}.}
    
    \item{household:}{output of \code{function(u) is.finite(u) & (u==0)}}
    \item{local:}{output of \code{function(u) is.finite(u) & (u<0.0625)}}
    \item{global:}{output of \code{function(u) (u>=0.0625) & is.finite(u)}}
    \item{nothousehold:}{output of \code{function(u) is.finite(u) & (u>0)}}
  }

  The time variables describe the transitions of the individual in an
  Susceptible-Exposed-Infectious-Revovered (SEIR) model.  Note that in
  order to avoid ties in the event times resulting from the daily
  interval censoring, a \eqn{U(0,1)} distributed random variable has
  been subtracted from \code{tS}, \code{tD} and \code{tQ}.

  The \code{twinSIR} data set \code{hagelloch} contains a number of
  extra entries created by giving an f argument to the \code{as.epidata}
  conversion. This generates indicator variables based on the distance
  \eqn{u}{u} between events. We use 62.5 meter as local distance, see
  p.255 of Neal & Roberts (2004) for details.
  
}

\source{
  Thanks to Peter J. Neal, University of Manchester, for providing us
  with these data, which he again became from Niels Becker, Australian
  National University. To cite the data, the main references are Pfeilsticker
  (1863) and Oesterle (1992). 
}

\examples{
data("hagelloch")

### Data frame with the original data documented in Oesterle (1992)

head(hagelloch.df)

# Show case locations as in Neal & Roberts (different scaling) using
# the data.frame (promoted to a SpatialPointsDataFrame)
coordinates(hagelloch.df) <- c("HNX","HNY")
plot(hagelloch.df, xlab="x [m]", ylab="x [m]", pch=15, axes=TRUE,
     cex=sqrt(multiplicity(hagelloch.df)))

# Epicurve
hist(as.numeric(hagelloch.df$tS), xlab="Time (days)", ylab="Cases", main="")


### SIR model information for population & individuals

(s <- summary(hagelloch))
plot(s, col=c("green","red","darkgray"))
stateplot(s, id=c("187"))

\dontrun{
  # Show a dynamic illustration of the spread of the infection
  animate(hagelloch,time.spacing=0.1,legend.opts=list(x="topleft"),sleep=1/100)
}
}

\references{
  Pfeilsticker, A. (1863). Beitr\enc{ä}{ae}ge zur Pathologie der Masern mit
  besonderer Ber\enc{ü}{ue}cksichtigung der statistischen Verh\enc{ä}{ae}ltnisse,
  M.D. Thesis, Eberhard-Karls-Universit\enc{ä}{ae}t T\enc{ü}{ue}bingen.
  Available as \url{http://www.archive.org/details/beitrgezurpatho00pfeigoog}.
  
  Oesterle, H. (1992). Statistische Reanalyse einer Masernepidemie 1861
  in Hagelloch, M.D. Thesis, Eberhard-Karls-Universit\enc{ä}{ae}at
  T\enc{ü}{ue}bingen.

  Neal, P. J. and Roberts, G. O (2004). Statistical inference and model
  selection for the 1861 Hagelloch measles epidemic, Biostatistics
  5(2):249-261
}

\seealso{\code{\link{twinSIR}}, \code{\link{epidata}}}

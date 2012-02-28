\name{imdepi}
\docType{data}
\alias{imdepi}
\encoding{latin1}

\title{Occurrence of invasive meningococcal disease in Germany}
\description{Data on the spatio-temporal location of 636 cases of two
  specific finetypes of invasive meningococcal diseases in
  Germany. 
}
\usage{imdepi}
\format{
  Loading the data in \code{imdepi} gives an object of type
  \code{epidataCS} with entries \code{events}, \code{stgrid}, \code{W}
  and \code{qmatrix}. See 'details' for further information.
}
\details{
  The dataset contains a slightly modified version of the data used in
  Meyer et al. (2010) represented as an \code{epidataCS} object. 

  The \code{events} entry is a \code{SpatialPointsDataFrame} object
  (ETSR89 projection, i.e. EPSG code 3035, with unit 'km') containing
  the 636 events, each with the following entries:
  \describe{
    \item{ID}{Unique identifier of the case.}
    \item{time}{Time of the case occurrence measured in number of days since origin.}
    \item{tile}{Tile number in the spatio-temporal covariate grid, which
      the event is contained in. This corresponds to one of the 413
      districts of Germany..
    }
    \item{type}{Event finetype, a factor with levels 'B' and 'C'}
    \item{eps.t}{Maximal temporal interaction range for the event.}
    \item{eps.s}{Maximal spatial interaction range for the event.}
    \item{sex}{Sex of the case, i.e. a factor with levels 'male' or 'female'.}
    \item{agegrp}{Factor giving the age group of the case,
      i.e. 0-2, 3-18 and >=19. Note: for a a number of cases this
      information is not available (\code{NA}).}
    \item{start}{Start time (in days since origin) of the cell in the
      spatio-temporal endemic covariate grid, which the event belongs to.}
    \item{popdensity}{Population density at the location of the event
      (corresponds to population density of the district where the event is
      located).}
  }
  
  With \code{coordinates(imdepi$events)} one obtains the (x,y) location
  of the events. The identifier in \code{tile} is indexed according to
  the German official municipality key ('Amtlicher
  Gemeindeschl\enc{ü}{ue}ssel'). See
  \url{https://secure.wikimedia.org/wikipedia/de/wiki/Amtlicher_Gemeindeschl\%C3\%BCssel} for details.

    
  Entry \code{stgrid} contains the spatio-temporal grid of endemic
  covariate information. In addition to the usual bookeeping variables
  this includes:
  \describe{
    \item{area}{Area of the district according to }
    \item{popdensity}{Population density computed from DESTATIS
      (Federal Statistical Office) information (Date: 31.12.2008) on
      communities level (LAU2) aggregated to district level (NUTS3).}
  }
  
  The entry \code{W} contains the observation windows as a
  \code{SpatialPolygons} object. In this case the boundaries of
  Germany (simplified polygon to enhance speed).
  
  The entry \code{qmatrix} is a \eqn{2\times 2}{2 x 2} identity matrix
  indicating that no transmission between the two finetypes can occur.
}

\source{
  National Reference Centre for Meningococci (NRZM) -- hosted by the
  Department of Hygiene and Microbiology,
  Julius-Maximilians-Universit\enc{ä}{ae}t W\enc{ü}{ue}rzburg, Germany.
  Thanks to Dr. Johannes Elias and Prof. Dr. Ulrich Vogel from the NRZM
  for providing the IMD event data. See
  \url{http://www.meningococcus.de/}
  and \url{http://episcangis.hygiene.uni-wuerzburg.de/} for further
  details about the IMD event data.
}

\references{
  S. Meyer, J. Elias and M. H\enc{ö}{oe}hle (2011), A space-time conditional
  intensity model for invasive meningococcal, Biometrics, Epub ahead of
  print. Note: A technical report is available as
  http://epub.ub.uni-muenchen.de/11898/ 
}

\examples{
  data("imdepi")

  #Basic information
  imdepi
  head(imdepi,n=2)

  #Event data
  marks(imdepi)

  #Summary
  s <- summary(imdepi)
  str(s)

  # type frequencies
  s$typeTable

  # weekly step function of number of infectives
  plot(s$counter, xlab = "Time [days]", ylab = "Number of infected individuals", main = "Time series of IMD")

  # distribution of number of potential sources of infection
  oldpar <- par()
  par(mfrow=c(1,2), las=1)
  for (type in c("B","C")) {
    plot(100*prop.table(table(s$nSources[s$eventType==type])),
    xlim=range(s$nSources), xlab = "Number of potential epidemic sources",
    ylab = "Proportion of events [\%]")
  }
  par(oldpar)

  #Plot the spatial dimension of the object
  plot(imdepi$W)
  col <- c("darkblue","indianred2")
  pch <- c(3,4)
  with(imdepi, {
    plot(events,pch=pch[events$type],col=col[events$type],add=TRUE)
    legend(x="topleft",levels(events$type),col=col, pch=pch,title="Finetype")
  })

  \dontrun{
    #Show a dynamic illustration of the spatio-temporal dynamics of the
    #spread using the animation package
    require("animation")
    ani.options(interval = 0, nmax = 999)

    #Create an epidataCS subset containing only a single type
    imdepi.type <-  lapply(c("B","C"), function(type) {
        foo <- imdepi
        foo$events <- foo$events[foo$events$type == "B",]
       return(foo)
    })

    #Animate one of them
    animate(imdepi.type[[1]], time.spacing = 10, pch = c(15,17),
                          main="IMD dynamics", axes=FALSE, xlab="", ylab="")

  }
}

\seealso{\code{\link{epidataCS}}}
\keyword{datasets}


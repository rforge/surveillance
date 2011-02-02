################################################################################
### Initialisation of the R session for the development of twinstim.
### Author: Sebastian Meyer
### Description: Determine some basic options and path settings, load packages.
################################################################################

### Suppress automatic conversion of character variables to factors
options(stringsAsFactors = FALSE)

### Activate EPS settings for postscript production
setEPS()

### For multiple lattice plots, go left to right, top to bottom by default
library("lattice")
latticeDefaults <- lattice.getOption("default.args")
latticeDefaults$as.table <- TRUE
lattice.options(default.args = latticeDefaults)

### Load packages
library("RLadyBug")
library("RColorBrewer") # "good" colour palettes
library("xtable")       # generate LaTeX code
library("sp")           # classes for spatial data
library("rgdal")        # provides acces to the Geospatial Data Abstraction
                        # Library (raster data), OGR (vector data) and to PROJ.4
library("gpclib")       # general polygon clipping library
library("intervals")    # tools for points and intervals
library("statmod")      # contains gauss.quad for nodes and weights calculation
library("spatstat")     # Spatial Point Pattern analysis, model-fitting,
                        # simulation, tests
spatstat.options(gpclib = TRUE)  # may use gpclib, as thesis is non-comercial
library("animation")

### Little helper
sourceDir <- function(path, trace = TRUE, ...)
{
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}

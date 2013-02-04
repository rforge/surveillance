################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[fluBYBWmap-fix.R] by SM Mon 04/02/2013 20:16 (CET)>
### Description:
### Up to rev. 504, data(fluBYBW) had an error in the map slot:
### LK Unterallgäu (9778) was missing a tiny polygon part
################################################################################

## CAVE: this script only works for data(fluBYBW) <= rev. 504

data("fluBYBW", package="surveillance")
library("maptools")
maptools::gpclibPermit()

fixmap <- function () {
    W <- maptools::unionSpatialPolygons(fluBYBW@map,
                                        IDs = rep.int(1,length(fluBYBW@map@polygons)),
                                        avoidGEOS = TRUE)
    plot(fluBYBW@map)
    plot(W, add=TRUE, border=2, lwd=2)

    ## add missing polygon to district 9778
    missingpoly <- W@polygons[[1]]@Polygons[2]
    missingpoly[[1]]@hole <- FALSE
    missingpoly[[1]]@ringDir <- 1L
    fluBYBW@map@polygons[[match("9778", row.names(fluBYBW@map))]]@Polygons <-
        c(fluBYBW@map@polygons[[match("9778", row.names(fluBYBW@map))]]@Polygons,
          missingpoly)
    
    plot(fluBYBW@map["9778",], add=TRUE, border=4)
    fluBYBW@map
}

fixedmap <- fixmap()

plot(fixedmap["9778",], col=2)
plot(fluBYBW@map["9778",], add=TRUE, col=rgb(0.9,0.9,0.9,0.9))
all.equal(fixedmap["9778",], fluBYBW@map["9778",])

## also remove redundant SP_ID columns
all.equal(fixedmap[["SP_ID_1"]], fixedmap[["SP_ID"]])
all.equal(fixedmap[["id"]], as.integer(as.character(fixedmap[["SP_ID"]])))
all.equal(fixedmap[["id"]], as.integer(row.names(fixedmap)))
fixedmap$SP_ID_1 <- fixedmap$SP_ID <- NULL

## overwrite previous map
fluBYBW@map <- fixedmap

################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[districtsD.R] by SM Mon 31/03/2014 17:17 (CEST)>
### Project: check simplified district polygons
################################################################################

library("sp")
load("kreise.RData")
load("kreiseSimple.RData")
oldSimple <- local({
    load("~/Projekte/twinstim/data/BKG-GDZ/simplified/districtsD-sV60.RData")
    districtsD[order(row.names(districtsD)),]
})
summary(kreise)
summary(kreiseSimple[[1]])
summary(oldSimple)

## object size
object.size(kreise)
sapply(kreiseSimple, object.size)
object.size(oldSimple)  # smaller because no comment()s on member "Polygons"
##sapply(kreiseSimple[[1]]@polygons, comment)

## area preservation
areas.orig <- sapply(kreise@polygons, slot, "area")
par(mfrow=n2mfrow(length(kreiseSimple)+1))
lapply(c(kreiseSimple, oldSimple), function (x)
       plot(areas.orig, sapply(x@polygons, slot, "area"),
            ylim=range(areas.orig), panel.first=abline(0,1,col=2)))
## area quality of old simplification is similar to new 6.6% simplification

## plot
par(mfrow=n2mfrow(length(kreiseSimple)), mar=c(0,0,0,0))
lapply(kreiseSimple, plot)

## previous "districtsD" is close to new mV6.6
par(mfrow=c(1,2), mar=c(0,0,0,0))
plot(oldSimple)
plot(kreiseSimple[["mV6.6"]])


### check simplified district polygons against imdepi event locations

load("~/Projekte/twinstim/mydata/imdepi-tied.RData")

check_tiles_events <- function (tiles, events)
{
    tiles <- as(tiles, "SpatialPolygons") # remove potential data for over()
    stopifnot(inherits(events, "SpatialPointsDataFrame"),
              identicalCRS(tiles, events))
    ## get polygon ID's of events (via overlay)
    eventtiles <- row.names(tiles)[over(events, tiles)]
    
    if (length(which_not_in_tiles <- which(is.na(eventtiles))))
        warning("some of 'events' are not within 'tiles': ",
                paste(which_not_in_tiles, collapse=", "))

    if (!is.null(events@data[["tile"]])) {
        which_disagree <- setdiff(
            which(eventtiles != as.character(events$tile)),
            which_not_in_tiles)
        if (length(which_disagree))
            warning("'over(events, tiles)' disagrees with 'events$tile': ",
                    paste(which_disagree, collapse=", "))
    }
}

options(warn=1)
invisible(lapply(kreiseSimple, check_tiles_events, events=imdepi$events))
check_tiles_events(oldSimple, imdepi$events)
## for simplification levels 1% and 1.6% some events fall outside the window
## at all levels, some events are not located in events$tile

## areas
stopifnot(all.equal(kreise$AREA,
                    imdepi$stgrid$area[seq_len(nlevels(imdepi$stgrid$tile))]))
areas.simple <- sapply(kreiseSimple, function (x)
                       sapply(x@polygons, slot, "area"))
apply(cbind(areas.simple, areas.orig), 2, all.equal, kreise$AREA)
## even for original "kreise" we have a mean relative difference of 0.01222161


### choose simplification level 6.6%

districtsD <- kreiseSimple[["mV6.6"]]
stateD <- rgeos::gUnaryUnion(districtsD)
row.names(stateD) <- "D"
plot(districtsD)
plot(stateD, add=TRUE, border=2, lwd=2)


### save

save(districtsD, stateD,
     file="~/Projekte/surveillance/pkg/inst/shapes/districtsD.RData",
     compress="xz")

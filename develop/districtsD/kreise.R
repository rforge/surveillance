################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[kreise.R] by SM Mon 31/03/2014 10:26 (CEST)>
### Project: take 'kreise' "SpatialPolygonsDataFrame" from Biometrics paper
################################################################################

## basically this is the shapefile "vg2500_krs" obtained from
## www.geodatenzentrum.de as at 2009-01-01,
## with additional columns obtained via aggregation of municipality data of the
## "Gemeindeverzeichnis GV 2000" (February 2009),
## and spTransform()ed to CRS("+init=epsg:3035 +units=km")

library("sp")
kreise <- local({
    load("~/Projekte/twinstim/mydata/data.Rdata")
    kreise
})


## remove useless columns
kreise$USE <- kreise$RS <- kreise$SHAPE_LENG <- kreise$SHAPE_AREA <- NULL
## also remove gender-specific population counts and population density
kreise$MALES <- kreise$FEMALES <- kreise$POPDENSITY <- NULL

## use official municipality key as row.names
row.names(kreise) <- kreise$KEY

## order by key
kreise <- kreise[order(row.names(kreise)),]

## summary
summary(kreise)

## save
save(kreise, file="kreise.RData")
tools::resaveRdaFiles("kreise.RData")

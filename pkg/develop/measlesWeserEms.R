################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[measlesWeserEms.R] by SM Fre 22/11/2013 16:19 (CET)>
### Project: Update "measles.weser" data and save as "sts" object
### Description: Add map and two missing districts,
###              correct one observed count,
###              rename district keys (0....)
################################################################################


### load old "disProg" data obtained from SurvStat@RKI as of Jahrbuch 2004

library("surveillance")
data("measles.weser")
str(measles.weser)  # old "disProg"-class


### observed counts

observed <- measles.weser$observed

## add initial zeroes to district keys
colnames(observed) <- paste0(0,colnames(observed))

## add two missing districts with zero counts:
## SK Delmenhorst (03401) and SK Wilhemshaven (03405)
observed <- cbind(observed, "03401"=0, "03405"=0)

## order by key
observed <- observed[,order(colnames(observed))]

## Correction: as of Jahrbuch 2005, there is one more case attributed to
##             LK Oldenburg (03458) during 2001/W17, i.e. 2 cases instead of 1.
cbind("2001" = colSums(observed[1:52,]),
      "2002" = colSums(observed[53:104,]))
observed[17,"03458"] <- 2


### map (from BKG-GDZ, data as of 01.01.2009, 30% Douglas-Peucker reduction)

library("rgdal")
districtsD <- readOGR("/home/sebastian/Projekte/twinstim/data/BKG-GDZ",
                      "vg2500_krs-polygons-ms30", encoding="latin1")
row.names(districtsD) <- substr(districtsD$RS, 1, 5)
names(districtsD@data) <- sub("SHAPE_AREA", "AREA", names(districtsD@data))
weserems_map <- districtsD[colnames(observed), c("GEN","AREA")]
plot(weserems_map, axes=TRUE)

## clarify district names since non-unique (prepend SK/LK)
weserems_map$GEN <- sub(" (Oldenburg)", "", weserems_map$GEN, fixed=TRUE)
weserems_map$GEN <- paste(
    ifelse(row.names(weserems_map) %in% paste0(0,3401:3405), "SK", "LK"),
    weserems_map$GEN,
    sep=" ")

## for maximum portability, we should only use ASCII characters
weserems_map$GEN <- gsub("ü", "ue", weserems_map$GEN)
stopifnot(length(tools::showNonASCII(weserems_map$GEN)) == 0)

## add population (numbers are ordered by district key)
## NLS, 31.12.2003 (obtained from http://www.lskn.niedersachsen.de/portal/live.php?navigation_id=25688&article_id=87679&_psmand=40)
weserems_map$POPULATION <- c(75986, 51445, 158340, 165517, 84586, 114524,
                             189652, 153283, 307734, 101657, 132975, 164540,
                             124564, 358041, 130471, 94242, 57672)
## population data for earlier years are not available online
## DESTATIS, 31.12.2012
## weserems_map$POPULATION2012 <- c(73588, 49751, 158658, 155625, 76545, 118489,
##                                  186673, 160033, 312855, 97327, 133652, 164202,
##                                  125413, 350444, 133462, 89126, 56362)

## compare to fractions of old measles.weser data
## (missing Delmenhorst and Wilhelmshaven)
oldpopfrac <- measles.weser$populationFrac[1,]
oldpopfrac <- oldpopfrac[order(names(oldpopfrac))]
idx <- row.names(weserems_map) %in% paste0(0,names(oldpopfrac))
comppopfrac <- cbind(oldpopfrac,
                     weserems_map$POPULATION[idx] / sum(weserems_map$POPULATION[idx]))
comppopfrac
stopifnot(all.equal(comppopfrac[,1], comppopfrac[,2], tolerance=0.005))


### neighbourhood matrix

weserems_nb <- poly2adjmat(weserems_map)
stopifnot(isTRUE(all.equal(
    measles.weser$neighbourhood,
    weserems_nb[-c(1,5),-c(1,5)],
    check.attributes=FALSE)))
weserems_nborder <- nbOrder(weserems_nb, maxlag=10)


### generate sts

measlesWeserEms <- new("sts",
                       epoch=seq_len(nrow(observed)), freq=52, start=c(2001, 1),
                       observed=observed, neighbourhood=weserems_nborder,
                       populationFrac=prop.table(weserems_map$POPULATION),
                       map=weserems_map, epochAsDate=FALSE, multinomialTS=FALSE)

save(measlesWeserEms, file="../data/measlesWeserEms.RData")
tools::resaveRdaFiles("../data/measlesWeserEms.RData")

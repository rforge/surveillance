library("maptools")
library("surveillance")

######################################################################
# Create influenza data for Bavaria and Baden-Wuerttemberg
######################################################################

# read in observed number of cases
flu.counts <- as.matrix(read.table("flu_ByBw.txt"))
namesLK <- substring(colnames(flu.counts),first=2,last=100)
colnames(flu.counts) <- namesLK
# population sizes
p <- as.matrix(read.table("population_2001-12-31_ByBw.txt"))
# read in adjacency matrix with elements 1 if two regions share a common border
nhood <- as.matrix(read.table("neighourhood_ByBw.txt"))
map <- readShapePoly("../inst/shapes/districts_BYBW.shp", IDvar = "id")

#Create the sts object
fluBB <- new("sts", epoch = 1:nrow(flu.counts),
           observed = flu.counts,
           start = c(2001, 1),
           freq = 52,
           neighbourhood = nhood,
           map = map,
           population = p
           )

#Spatial plot showing the number of cases in each region
plot(flu[year(fluBB) == 2001, ], type= observed ~ 1 | unit , labels = FALSE)

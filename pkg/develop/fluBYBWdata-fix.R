################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[fluBYBWdata-fix.R] by SM Sam 06/07/2013 11:44 (CEST)>
### Description:
### Up to rev. 583, data(fluBYBW) had an error in @observed: the very last row
### contained only zeroes as an artifact from RKI export. 
### It corresponded to week 53 of the year 2008 but there was no such week.
################################################################################


library("surveillance")
data("fluBYBW", package="surveillance")


### remove last week of 2008, which is an empty artifact of RKI's default
### 53 weeks table

if (sum(fluBYBW@observed[nrow(fluBYBW@observed),]) == 0) {
    fluBYBW <- fluBYBW[seq_len(nrow(fluBYBW)-1),]
}

## CAVE: 2004 really had 53 (!) ISO weeks. Michaela assigned the 13 cases of
## this extra week to the first week of 2005, which thus has 25 (not 12) cases
rowSums(fluBYBW@observed[do.call("seq",as.list(match(2004:2005,year(fluBYBW)))),])

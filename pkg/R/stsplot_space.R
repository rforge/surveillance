################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Snapshot map (spplot) of an sts-object or matrix of counts
###
### Copyright (C) 2013-2014 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

## x: "sts" or (simulated) matrix of counts
## tps: one or more time points. The unit-specific _sum_ of time points "tps" is
##      plotted. tps=NULL means cumulation over all time points in x.
## at: number of levels for the grouped counts or specific break points to
##     use, or list(n, data, trafo) passed to getCountIntervals(),
##     where data and trafo are optional.
##     CAVE: intervals are closed on the left and open to the right.
##     From panel.levelplot: zcol[z >= at[i] & z < at[i + 1]] <- i
##     i.e. at=0:1 will have NA (=also white) for counts=1, thus we have to
##     ensure max(at) > max(counts)

stsplot_space <- function (x, tps = NULL, map = x@map,
                           population = NULL, # nUnits-vector of population counts
                           main = NULL, at = 10, col.regions = NULL,
                           colorkey = list(space="bottom", labels=list(at=at)),
                           total.args = NULL, sp.layout = NULL, ...)
{
    counts <- if (inherits(x, "sts")) observed(x) else x
    map <- map[colnames(x),] # assure same order

    ## compute data to plot
    ncases <- getCumCounts(counts, tps)
    total <- sum(ncases)
    if (!is.null(population)) { # plot prevalence
        stopifnot(is.vector(population, mode="numeric"),
                  length(population) == ncol(counts))
        if (!is.null(colnames(counts)) && !is.null(names(population)))
            stopifnot(colnames(counts) == names(population))
        ncases <- ncases / population
        total <- total / sum(population)
    }
    
    ## check/determine break points 'at'
    at <- checkat(at, ncases)

    ## add ncases to map@data
    if (inherits(map, "SpatialPolygonsDataFrame")) {
        map$ncases <- ncases
    } else {
        map <- SpatialPolygonsDataFrame(map, as.data.frame(ncases))
    }
    
    if (is.null(col.regions)) {
        separate0 <- at[1] == 0 & at[2] <= 1
        col.regions <- c(
            if (separate0) "white",
            hcl.colors(ncolors=length(at)-1-separate0,
                       use.color=TRUE))
    }
    if (!missing(colorkey) && is.list(colorkey))
        colorkey <- modifyList(eval(formals()$colorkey), colorkey)
    if (is.null(main) && inherits(x, "sts"))
        main <- stsTimeRange2text(x, tps)

    if (is.list(total.args) && require("grid")) {
        total.args <- modifyList(list(label="Overall: ", x=1, y=0), total.args)
        if (is.null(total.args$just))
            total.args$just <- with (total.args, if (all(c(x,y) %in% 0:1)) {
                c(c("left", "right")[1+x], c("bottom","top")[1+y])
            } else "center")
        total.args$label <- paste0(total.args$label, round(total,1))
        layout.total <- c("grid.text", total.args)
        sp.layout <- c(sp.layout, list(layout.total))
    }
    args <- list(quote(map), "ncases", main=main,
                 col.regions=col.regions, at=at, colorkey=colorkey,
                 sp.layout=sp.layout, quote(...))
    do.call("spplot", args)
}



#######################################################
### Auxiliary functions for the "sts" snapshot function
#######################################################

getCumCounts <- function (counts, tps=NULL, nUnits=ncol(counts))
{
    if (!is.null(tps)) counts <- counts[tps,,drop=FALSE]
    ntps <- nrow(counts)
    if (ntps == 1) c(counts) else .colSums(counts, ntps, nUnits)
}

checkat <- function (at, data) { # "data" should be on the original scale
    if (isScalar(at))
        at <- list(n=at)
    at <- if (is.list(at)) {
        at <- modifyList(list(n=10, data=data), at)
        do.call("getCountIntervals", at)
    } else sort(at) 
    if (any(data >= max(at) | data < min(at), na.rm=TRUE))
        stop("'at' (right-open!) does not cover the data (range: ",
             paste0(format(range(data)), collapse=" - "), ")")
    structure(at, checked=TRUE)
}

getCountIntervals <- function (nInt, data, trafo=scales::sqrt_trans(), ...) {
    maxcount <- max(data, na.rm=TRUE)
    if (maxcount < nInt) { # no aggregation of counts necessary
        at <- 0:ceiling(maxcount+sqrt(.Machine$double.eps)) # max(at) > maxcount
    } else {
        at <- if (requireNamespace("scales", quietly=TRUE)) {
            scales::trans_breaks(trafo$trans, trafo$inv, n=nInt+1, ...)(data)
        } else pretty(sqrt(data), n=nInt+1, ...)^2
        if (at[1] == 0 & at[2] > 1) # we want 0 counts separately ("white")
            at <- sort(c(1, at))
        if (at[length(at)] == maxcount) # ensure max(at) > maxcount
            at[length(at)] <- at[length(at)] + 1
    }
    at
}

stsTime2text <- function (stsObj, tps=TRUE)
    paste(year(stsObj)[tps], epochInYear(stsObj)[tps], sep="/")

stsTimeRange2text <- function (stsObj, tps=NULL)
{
    tpsRange <- if (is.null(tps)) c(1, nrow(stsObj)) else range(tps)
    tpsRangeYW <- stsTime2text(stsObj, tpsRange)
    paste(unique(tpsRangeYW), collapse=" - ")
}

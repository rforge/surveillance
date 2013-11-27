################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Plot-method for fitted hhh4() models
###
### Copyright (C) 2010-2013 Michaela Paul and Sebastian Meyer
### $Revision$
### $Date$
################################################################################


plot.ah4 <- function (x, type=c("fitted", "ri"), ...)
{
    cl <- match.call()
    cl$type <- NULL
    cl[[1L]] <- as.name(paste("plotHHH4", match.arg(type), sep="_"))
    eval(cl, envir=parent.frame())
}


plotHHH4_fitted <- function (x, units = 1, names = NULL,
                             col = c("orange","blue","grey85","black"),
                             pch = 19, pt.cex = 0.6,
                             par.settings = list(),
                             legend = TRUE, legend.args = list(),
                             legend.observed = TRUE, ...)
{
    if (!is.null(names)) stopifnot(length(units) == length(names))
    if (length(col) == 3) col <- c(col, "black") else if (length(col) != 4)
        stop("'col' must be of length 3 or 4") # for backwards compatibility

    if (is.list(par.settings)) {
        par.defaults <- list(mfrow = sort(n2mfrow(length(units))),
                             mar = c(4,4,2,0.5)+.1, las = 1)
        par.settings <- modifyList(par.defaults, par.settings)
        opar <- do.call("par", par.settings)
        on.exit(par(opar))
    }

    ## legend options
    if (is.logical(legend)) legend <- which(legend)
    if (!is.list(legend.args)) {
        if (length(legend) > 0)
            warning("ignored 'legend' since 'legend.args' is not a list")
        legend <- integer(0L)
    }
    if (length(legend) > 0) {
        legendidx <- 1L + c(if (legend.observed) 0L,
                            which(sapply(x$control[c("ne","ar","end")],
                                         "[[", "inModel")))
        default.args <- list(
            x="topright", col=c(col[4],col[1:3])[legendidx], lwd=6,
            lty=c(NA,1,1,1)[legendidx], pch=c(pch,NA,NA,NA)[legendidx],
            pt.cex=pt.cex, pt.lwd=1, bty="n", inset=0.02,
            legend=c("observed","spatiotemporal","autoregressive","endemic")[legendidx]
            )
        legend.args <- modifyList(default.args, legend.args)
    }

    ## plot fitted values region by region
    meanHHHunits <- vector(mode="list", length=length(units))
    for(i in seq_along(units)) {
        meanHHHunits[[i]] <- plotHHH4_fitted1(x, units[i], main=names[i],
                                              col=col, pch=pch, pt.cex=pt.cex,
                                              ...)
        if (i %in% legend) do.call("legend", args=legend.args)
    }
    invisible(meanHHHunits)
}

## plot estimated component means for a single region
plotHHH4_fitted1 <- function(x, unit=1, main=NULL,
                             col=c("grey30","grey60","grey85","grey0"),
                             pch=19, pt.cex=0.6, border=col,
                             start=x$stsObj@start, end=NULL, ylim=NULL,
                             xlab="", ylab="No. infected", 
                             hide0s=FALSE, meanHHH=NULL)
{
    stsObj <- x$stsObj
    if (is.character(unit) &&
        is.na(unit <- match(.unit <- unit, colnames(stsObj))))
        stop("region '", .unit, "' does not exist")
    if (is.null(main)) main <- colnames(stsObj)[unit]

    ## get observed counts
    obs <- observed(stsObj)[,unit]
    if (is.null(ylim)) ylim <- c(0, max(obs,na.rm=TRUE))

    ## time range for plotting
    timevec2point <- function (timevec, frequency = stsObj@freq, toleft=FALSE)
        timevec[1L] + (timevec[2L] - toleft)/frequency
    start0 <- timevec2point(stsObj@start, toleft=TRUE) # start of the sts object
    start <- timevec2point(start)
    if (start < start0) stop("'start' is before 'x$stsObj@start'")
    tp <- start0 + seq_along(obs)/stsObj@freq
    tpsubset <- tp[x$control$subset]
    end <- if (is.null(end)) tp[length(tp)] else timevec2point(end)
    stopifnot(start < end)

    ## get fitted component means
    if (is.null(meanHHH))
        meanHHH <- meanHHH(coef(x,reparamPsi=FALSE), terms(x))
    meanHHHunit <- sapply(meanHHH, "[", i=TRUE, j=unit)
    
    ## establish basic plot window
    plot(c(start,end), ylim, xlab=xlab, ylab=ylab, type="n")
    title(main=main, line=0.5)

    ## draw polygons
    xpoly <- c(tpsubset[1], tpsubset, tail(tpsubset,1))
    polygon(xpoly, c(0,meanHHHunit[,"mean"],0),
            col=col[1], border=border[1])
    if (x$control$ar$inModel)
        polygon(xpoly, c(0,rowSums(meanHHHunit[,c("endemic","epi.own")]),0),
                col=col[2], border=border[2])
    if (x$control$end$inModel)
        polygon(xpoly, c(0,meanHHHunit[,"endemic"],0),
                col=col[3], border=border[3])

    ## add observed counts
    ptidx <- if (hide0s) obs > 0 else TRUE
    points(tp[ptidx], obs[ptidx], col=col[4], pch=pch, cex=pt.cex)

    ## invisibly return the fitted component means for the selected region
    invisible(meanHHHunit)
}


plotHHH4_ri <- function (x, component, sp.layout = NULL,
                         gpar.missing = list(col="darkgrey", lty=2, lwd=2),
                         ...)
{
    ranefmatrix <- ranef(x, tomatrix=TRUE)
    if (is.null(ranefmatrix)) stop("model has no random effects")
    stopifnot(length(component) == 1L)
    if (is.na(comp <- pmatch(component, colnames(ranefmatrix))))
        stop("'component' must (partially) match one of ",
             paste(dQuote(colnames(ranefmatrix)), collapse=", "))
    
    map <- x$stsObj@map
    if (is.null(map)) stop("'x$stsObj' has no map")
    map$ranef <- ranefmatrix[,comp][row.names(map)]
    
    if (is.list(gpar.missing) && any(is.na(map$ranef)))
        sp.layout <- c(sp.layout, 
                       c(list("sp.polygons", map[is.na(map$ranef),]),
                         gpar.missing))
    spplot(map[!is.na(map$ranef),], zcol = "ranef",
           sp.layout = sp.layout, ...)
}

################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Plots for an array "hhh4sims" of simulated counts from an "hhh4" model,
### or a list thereof as produced by different "hhh4" models (same period!)
###
### Copyright (C) 2013-2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

plot.hhh4sims <- function (x, ...)
    plot.hhh4simslist(x, ...)

## time.hhh4sims <- function (x, ...)
##     as.integer(dimnames(x)[[1L]])


## class for a list of "hhh4sims" arrays
hhh4simslist <- function (x)
{
    attr(x, "stsObserved") <- attr(x[[1L]], "stsObserved")
    for (i in seq_along(x)) attr(x[[i]], "stsObserved") <- NULL
    class(x) <- "hhh4simslist"
    x
}


## converter functions
as.hhh4simslist <- function (x, ...) UseMethod("as.hhh4simslist")

as.hhh4simslist.hhh4sims <- function (x, ...) {
    if (nargs() > 1L) {
        as.hhh4simslist.list(list(x, ...))
    } else {
        hhh4simslist(list(x))
    }
}

as.hhh4simslist.list <- function (x, ...)
{
    ## verify class
    lapply(X = x, FUN = function (Xi)
        if (!inherits(Xi, "hhh4sims"))
            stop(sQuote("x"), " is not a list of ", dQuote("hhh4sims")))
    hhh4simslist(x)
}

as.hhh4simslist.hhh4simslist <- function (x, ...) x


####################
### plot methods ###
####################

plot.hhh4simslist <- function (x, type = c("size"), ...)
{
    cl <- sys.call()

    ## remove the "type" argument from the call
    if (is.null(names(cl)) && nargs() > 1L) { # unnamed call plot(x, type)
        cl[[3L]] <- NULL  # remove the second argument
    } else {
        cl$type <- NULL
    }
    
    cl[[1L]] <- as.name(paste("plotHHH4sims", match.arg(type), sep="_"))
    eval(cl, envir = parent.frame())
}


### simulated final size distribution as boxplots (stratified by units)

plotHHH4sims_size <- function (x, ...,
                               groups = NULL, par.settings = list())
{
    if (is.null(groups))
        return(plotHHH4sims_size_total(x, ...))
    
    x <- as.hhh4simslist(x)
    groups <- check_groups(groups, colnames(attr(x, "stsObserved")))
    
    if (is.list(par.settings)) {
        par.defaults <- list(mfrow = sort(n2mfrow(nlevels(groups))),
                             mar = c(4,4,2,0.5)+.1, las = 1)
        par.settings <- modifyList(par.defaults, par.settings)
        opar <- do.call("par", par.settings)
        on.exit(par(opar))
    }
    
    invisible(sapply(
        X = levels(groups),
        FUN = function (group) {
            idx_group <- which(group == groups)
            x_group <- x
            x_group[] <- lapply(X = x_group, FUN = "[",
                              , idx_group, , drop = FALSE)
            attr(x_group, "stsObserved") <- suppressMessages(
                attr(x_group, "stsObserved")[, idx_group])
            plotHHH4sims_size_total(x_group, ..., main = group)
        },
        simplify = FALSE, USE.NAMES = TRUE))
}

check_groups <- function (groups, units)
{
    if (isTRUE(groups)) {
        factor(units, levels = units)
    } else {
        stopifnot(length(groups) == length(units))
        as.factor(groups)
    }
}

### simulated final size distribution as boxplots aggregated over all units

plotHHH4sims_size_total <- function (x,
                                     horizontal = TRUE, trafo = NULL,
                                     label.observed = TRUE, ...)
{
    x <- as.hhh4simslist(x)
    nsims <- sapply(x, colSums, dims = 2) # sum over dims 1:2 (time-space)
    if (is.null(trafo)) trafo <- scales::identity_trans()
    nsimstrafo <- trafo$trans(nsims)
    
    ## default boxplot arguments
    fslab <- "Final size"
    if (trafo$name != "identity")
        fslab <- paste0(fslab, " (", trafo$name, "-scale)")
    defaultArgs <- list(ylab=fslab, yaxt="n", las=1, cex.axis=1, border=1)
    if (horizontal) names(defaultArgs) <- sub("^y", "x", names(defaultArgs))
    ## defaultArgs$mai <- par("mai")
    ## defaultArgs$mai[2] <- max(strwidth(boxplot.args$names, units="inches",
    ##                                    cex=boxplot.args$cex.axis))
    ## if (trafo$name != "identity") {
    ##     ## ?bxp: 'yaxs' and 'ylim' are used 'along the boxplot'
    ##     defaultArgs <- c(defaultArgs,
    ##                      list(ylim=c(0,max(nsimstrafo)*1.05), yaxs="i"))
    ## }

    ## generate boxplots
    boxplot.args <- modifyList(defaultArgs, list(...))
    boxplot.args$horizontal <- horizontal
    do.call("boxplot", c(list(nsimstrafo), boxplot.args))

    ## add means
    if (horizontal) {
        points(x=colMeans(nsimstrafo), y=1:ncol(nsimstrafo), pch=8, col=boxplot.args$border)
    } else points(colMeans(nsimstrafo), pch=8, col=boxplot.args$border)

    ## add axis
    aty <- pretty(nsims, n=par("lab")[2-horizontal])
    ##aty <- checkat(list(n=par("lab")[2], trafo=trafo), nsims) # linear on sqrt-scale
    axis(2-horizontal, at=trafo$transform(aty), labels=aty, las=boxplot.args$las)

    ## add line showing observed size
    observed <- observed(attr(x, "stsObserved"))
    if (horizontal) {
        abline(v=trafo$trans(sum(observed)), lty=2, lwd=2)
    } else {
        abline(h=trafo$trans(sum(observed)), lty=2, lwd=2)
    }
    if (label.observed)
        axis(2-horizontal, at=trafo$trans(sum(observed)),
             labels=sum(observed), font=2, lwd=2, las=boxplot.args$las,
             mgp=if (horizontal) c(3,0.4,0))

    ## numeric summary
    mysummary <- function(x)
        c(mean=mean(x), quantile(x, probs=c(0.025, 0.5, 0.975)))
    nsum <- t(apply(nsims, 2, mysummary))
    invisible(nsum)
}

################################################################################
# Authors: Sebastian Meyer, Michael Hoehle
# Date: 02 June 2009, modified 25 Mar 2011
#
# This file contains functions related to calculating and plotting intensities.
################################################################################


################################################################################
# Calculate the two components of the intensity lambda(t|H_t) for each row
# of the event history.
# Be aware that the function assumes atRiskY == 1 in all rows!
#
# ARGS:
#  theta - parameter vector c(alpha,beta), where
#          beta also contains the baseline coefficients in the first place
#  X     - covariate matrix related to alpha, i.e. the epidemic component
#  Z     - covariate matrix related to beta, i.e. the Cox-like endemic component
#
# RETURNS: a numeric matrix with two columns e and h and nrow(X)==nrow(Z) rows
################################################################################

.eh <- function(theta, X, Z)
{
    # Extracting params from theta
    dimX <- dim(X)
    nRows <- dimX[1] # = nrow(Z)
    px <- dimX[2]
    pz <- ncol(Z)
    alpha <- theta[seq_len(px)]
    beta <- theta[px + seq_len(pz)]
    
    # Calculate the epidemic component e(t|H_t) and the endemic component h(t)
    e <- if (px > 0L) drop(X %*% alpha) else numeric(nRows)
    h <- if (pz > 0L) drop(exp(Z %*% beta)) else numeric(nRows)
    
    # Return the two components of the infection intensity related to the
    # rows of the event history in a two column matrix
    eh <- cbind(e = e, h = h)
    return(eh)
}


################################################################################
# Cumulative hazard function
#
#      \Lambda(t) = \int_{timeRange[1]}^t \lambda(s) ds,
#
#  where \lambda(s) = \sum_{i=1}^n \lambda_i(s)
#
# Be aware that the function assumes atRiskY == 1 for all rows of X/Z/survs !!!
#
# ARGS:
#  t     - scalar time point until we want to integrate, must be non-negative
#  theta - parameter vector c(alpha,beta), where
#          beta also contains the baseline coefficients in the first place
#  X     - covariate matrix related to alpha, i.e. the epidemic component
#  Z     - covariate matrix related to beta, i.e. the Cox-like endemic component
#  survs - data.frame with columns id, start, stop, event; "timeRange" attribute
#  weights - vector of length nrow(X) indicating the number of individuals
#            with the same covariates. weights are allowed to change over time.
#            Note: it is assumed that none of the individuals covered by
#            "weights"  can have an actual event, if so they need to have their
#            own row
#
# RETURNS: value of the cumulative hazard function at time t
################################################################################

Lambda <- function(t, theta, X, Z, survs, weights)
{
    timeRange <- attr(survs, "timeRange")
    eh <- if (!isScalar(t) || t < timeRange[1L]) {
        stop("invalid argument 't': must be a scalar >= ", timeRange[1L],
             " (beginning of observation period)")
    } else if (t == timeRange[1L]) {
        return(0)
    } else if (t < timeRange[2L]) {
        # We have to extract the relevant intervals
        sortedStop <- sort(unique(survs$stop))
        # Find first stop time beyond t
        idx <- match(TRUE, sortedStop >= t)
        firstBeyondt <- sortedStop[idx]
        includeSurvsRow <- survs$stop <= firstBeyondt
        # If t between start and stop of an interval we need to chop...
        if (firstBeyondt != t) {
            survs$stop[survs$stop == firstBeyondt] <- t
        }
        # Extract relevant parts
        survs <- survs[includeSurvsRow,]
        weights <- weights[includeSurvsRow]
        .eh(theta, X[includeSurvsRow,], Z[includeSurvsRow,])
    } else { # if t >= attr(survs, "timeRange")[2], we take all rows
        .eh(theta, X, Z)
    }
    
    lambda <- rowSums(eh)
    dt <- survs$stop - survs$start
    intlambda <- sum(weights * lambda * dt)   # no individual sums as in loglik
    return(intlambda)
}



################################################################################
# Function to plot the path of the infection intensity or the proportions of
# the endemic or epidemic component, either on an individual basis or related
# to the total intensity at each event (=infection) time.
# The function works with objects of class "simEpidata"
# as well as with objects of class "twinSIR".
################################################################################

# if x is of class "twinSIR": theta = (alpha, beta) = (alpha, (h0coefs, betarest)) 
# if x is of class "simEpidata": theta = (alpha, 1, betarest)
# per default, the function uses the fitted or true parameters, respectively
intensityPlot <- function(x, type = c("overall", "individual"),
    what = c("epidemic proportion", "endemic proportion", "total intensity"),
    theta = NULL, plot = TRUE, add = FALSE, rug.opts = list(), ...)
{
    type <- match.arg(type)
    what <- match.arg(what)
    
    ## Extract model and theta
    if (inherits(x, "twinSIR")) {
        if (is.null(model <- x[["model"]])) {
            stop("'", deparse(substitute(x)), "' does not contain the 'model' ",
                 "component (use 'model = TRUE' when calling 'twinSIR')")
        }
        if (is.null(theta)) {
            theta <- coef(x)
        }
        end <- x$intervals[length(x$intervals)]
    } else if (inherits(x, "simEpidata")) {
        message("Note: the (true) baseline hazard is only evaluated",
                " at the beginning of the time intervals")
        model <- read.model(x)
        if (is.null(theta)) {
            theta <- c(attr(x,"config")$alpha, 1, attr(x,"config")$beta)
                                               # 1 is for true h0
        }
        end <- attr(x, "timeRange")[2L]
    } else {
        stop("'x' must inherit from classes \"twinSIR\" or",
             " \"simEpidata\"")
    }
    survs <- model$survs
    start <- attr(survs, "timeRange")[1L]
    timeIntervals <- unique(survs[c("start", "stop")])
    timepoints <- unique(c(timeIntervals$stop,end))
    # need 'end' here, because model does only contain rows with atRiskY == 1,
    # otherwise would terminate in advance if all individuals have been infected
    nTimes <- length(timepoints)
    idlevels <- levels(survs$id)
    
    # helper function for use with by()
    intensity <- function(iddata, what) {
        # 'iddata' will be a subset of survs, 'what' will be "wlambda" or "we"
        y <- numeric(nTimes)
        y[match(iddata$stop, timepoints)] <- iddata[[what]]
        y
    }
    
    ## Calculate epidemic (e) and endemic (h) component in each row of the model
    eh <- do.call(".eh", args = c(list(theta = theta), model[c("X", "Z")]))
    
    ## Calculate individual _total intensity_ paths
    lambda <- rowSums(eh)
    survs$wlambda <- as.vector(model$weights * lambda)
    # put individual intensity paths into a matrix [nTimes x n]
    wlambdaID <- by(data = survs, INDICES = list(id = survs$id),
                    FUN = intensity, what = "wlambda")
                    # would use simplify = FALSE here to omit the condition below,
                    # but this only exists since R 2.8.0 and the default was/is TRUE
    #hoehle@25.3.11 -- this does not work for hagelloch data where there is
    #an initial infectious person. Going back to slow but stable -> Sebastian 
#    wlambdaIDmatrix <- if (is.list(wlambdaID)) {
#                         as.matrix(as.data.frame(c(wlambdaID), optional = TRUE))
#                       } else { # wlambdaID is numeric, only if nTimes == 1
#                         t(as.matrix(wlambdaID))
#                       }
#     # alternative way but slower:
     wlambdaIDmatrix <- matrix(0, nrow = nTimes, ncol = length(idlevels),
                               dimnames = list(NULL, idlevels))
     for (ID in idlevels) {
         iddata <- subset(survs, subset = id == ID)
         wlambdaIDmatrix[match(iddata$stop, timepoints), ID] <- iddata$wlambda
     }
    
    if (what != "total intensity") {
        ## Calculate individual _epidemic intensity_ paths
        survs$we <- {
            px <- ncol(model$X)
            if (px == 0L) {
                stop("nothing to do, model does not contain both components")
            }
            as.vector(model$weights * eh[,1])
        }
        # put individual epidemic intensity paths into a matrix [nTimes x n]
        weID <- by(data = survs, INDICES = list(id = survs$id),
                   FUN = intensity, what = "we")
                   # would use simplify = FALSE here to omit the condition below,
                   # but this only exists since R 2.8.0 and the default was/is TRUE
        #hoehle@25.3.2011 - Does not work if initial infectious in data
#        weIDmatrix <- if (is.list(weID)) {
#                        as.matrix(as.data.frame(c(weID), optional = TRUE))
#                      } else { # weID is numeric, only if nTimes == 1
#                        t(as.matrix(weID))
#                      }
#     #-->hoehle: added alternative code which is slower, but works
        weIDmatrix <- matrix(0, nrow = nTimes, ncol = length(idlevels),
                              dimnames = list(NULL, idlevels))
        for (ID in idlevels) {
          iddata <- subset(survs, subset = id == ID)
          weIDmatrix[match(iddata$stop, timepoints), ID] <- iddata$we
        }
        ##end of bug fix -> cross check with Sebastian
    }
    
    ## Generate matrix with data for 'matplot'
    ydata2plot <-
        if (what == "total intensity") {
            if (type == "overall") {
                rowSums(wlambdaIDmatrix)
            } else {
                wlambdaIDmatrix
            }
        } else {   # calculate epidemic proportion
            if (type == "overall") {
                rowSums(weIDmatrix) / rowSums(wlambdaIDmatrix)
            } else {
                weIDmatrix / wlambdaIDmatrix
            }
        }
    if (what == "endemic proportion") {
        ydata2plot <- 1 - ydata2plot
    }
    ydata2plot <- as.matrix(ydata2plot)
    colnames(ydata2plot) <- if (type == "overall") what else idlevels
    
    if (what != "total intensity") {
        # there may be NAs in data2plot where the total intensity equals 0
        # => when calculating proportions we get 0 / 0 = NA
        # we redefine those values to 0. (0-intensity => 0-proportion)
        ydata2plot[is.na(ydata2plot)] <- 0
    }
    
    # prepend time (x) column
    data2plot <- cbind(stop = timepoints, ydata2plot)
    
    # if the epidemic is SIRS or SIS (re-susceptibility), there may be time
    # blocks during the observation period, where no individual is susceptible:
    # Problem: those time blocks are not included in the model component,
    #          which only contains rows with atRiskY == 1
    # Solution: fill the missing time periods with 0 intensity (or proportion)
    innerStart <- timeIntervals[-1L, "start"]
    innerStop <- timeIntervals[-nrow(timeIntervals), "stop"]
    noSusceptiblesStopTimes <- innerStart[innerStop != innerStart]
    if (length(noSusceptiblesStopTimes) > 0L) {
        data2plot <- rbind(data2plot,
            cbind(noSusceptiblesStopTimes,
                  matrix(0, nrow = length(noSusceptiblesStopTimes),
                            ncol = ncol(ydata2plot))
            )
        )
        data2plot <- data2plot[order(data2plot[,1L]),]
    }
    
    ## Plot and return data
    if (plot) {
        dotargs <- list(...)
        nms <- names(dotargs)
        if(! "xlab" %in% nms) dotargs$xlab <- "time"
        if(! "ylab" %in% nms) dotargs$ylab <- what
        if(! "pch" %in% nms) dotargs$pch <- 1
        if(! "lty" %in% nms) dotargs$lty <- 1
        do.call("matplot",
                args = c(list(x = c(start, data2plot[,1L]),
                              y = rbind(data2plot[1L, -1L, drop = FALSE],
                                        data2plot[  , -1L, drop = FALSE]),
                              type = "S", add = add),
                         dotargs))
        if (is.list(rug.opts)) {
            if (is.null(rug.opts$ticksize)) rug.opts$ticksize <- 0.02
            if (is.null(rug.opts$quiet)) rug.opts$quiet <- TRUE
            do.call("rug", args = c(list(x = attr(survs, "eventTimes")),
                                    rug.opts))
        }
        invisible(data2plot)
    } else {
        data2plot
    }
}

################################################################################
### Data structure for CONTINUOUS SPATIO-temporal infectious disease case data
### and a spatio-temporal grid of endemic covariates
###
### Author: Sebastian Meyer
### $Date: 2010-11-15 02:31:05 +0100 (Mon, 15 Nov 2010) $
################################################################################


######################################################################
# MAIN GENERATOR FUNCTION FOR epidataCS OBJECTS
# PARAMS:
# events: SpatialPointsDataFrame of cases with obligatory columns
#   time: time point of event
#   tile: reference to spatial unit (tile) in stgrid, where the event is located
#   type: optional type of event (-> marked twinstim). will be converted to a factor variable.
#   eps.t: maximal temporal influence radius (e.g. length of infectious period, time to culling, etc.), may be Inf
#   eps.s: maximal spatial influence radius (e.g. 100 [km]), may be Inf
#   The remaining columns are further marks of the event, e.g. sex, age of infected person (-> epidemic covariates)
#   The column names "ID", ".obsInfLength", ".bdist", ".influenceRegion", and ".sources" are reserved.
#   "ID": unique chronological ID for the events
#   ".obsInfLength": observed length of the infectious period (being part [0,T])
#   ".bdist": minimal distance of the event locations to the boundary
#   ".influenceRegion": object of class "owin", the intersection of W with b(s,eps.s), with origin at s
#   ".sources": potential sources of infection
# stgrid: data.frame with obligatory columns
#   tile: ID of spatial unit (e.g. id of municipality)
#   start, stop: temporal interval
#   area: area of the spatial unit (tile)
#   The remaining columns are endemic covariates.
#   The column name "BLOCK" is reserved (indexing the time intervals of stgrid).
# W: SpatialPolygons. Observation region. Must have same proj4string as events.
# qmatrix: square indicator matrix (0/1 or TRUE/FALSE) for possible transmission between the event types. will be internally converted to logical. Defaults to an independent spread of the event types.
# nCircle2Poly: accuracy (number of edges) of the polygonal approximation of a circle
######################################################################

obligColsNames_events <- c("time", "tile", "type", "eps.t", "eps.s")
obligColsNames_stgrid <- c("start", "stop", "tile", "area")

as.epidataCS <- function (events, stgrid, W, qmatrix = diag(nTypes), nCircle2Poly = 32)
{
    # Check and SORT events and stgrid
    cat("\nChecking 'events':\n")
    events <- checkEvents(events)
    cat("Checking 'stgrid':\n")
    stgrid <- checkstgrid(stgrid)

    # Check qmatrix
    cat("Checking 'qmatrix'...\n")
    typeNames <- levels(events$type)
    nTypes <- length(typeNames)
    qmatrix <- checkQ(qmatrix, typeNames)

    cat("Checking 'W' and 'nCircle2Poly'...\n")
    # Check class and proj4string of W
    stopifnot(inherits(W, "SpatialPolygons"),
              proj4string(W) == proj4string(events))
    # Check nCircle2Poly
    stopifnot(isScalar(nCircle2Poly))
    nCircle2Poly <- as.integer(nCircle2Poly)

    # Small helper function converting event index to (time, tile) string
    eventidx2string <- function (eventIdx) {
        paste(c("time", "tile", "type"), "=",
              unlist(events@data[eventIdx,c("time","tile","type")]),
              collapse = ", ")
    }

    # Check that all events are part of W
    cat("Checking if all events are part of 'W'...\n")
    WIdxOfEvents <- overlay(events, W)
    if (eventNotInWidx <- match(NA, WIdxOfEvents, nomatch = 0L)) {
        stop("the event at (", eventidx2string(eventNotInWidx), ") is not ",
             "inside 'W'")
    }

    # Some basic quantities
    eventCoords <- coordinates(events)
    nEvents <- nrow(eventCoords)
    timeRange <- with(stgrid, c(start[1], stop[length(stop)]))

    # Are event times covered by stgrid?
    cat("Checking if all events are covered by 'stgrid'...\n")
    if (events$time[1] <= timeRange[1] || events$time[nEvents] > timeRange[2]) {
        stop("event times are not covered by 'stgrid': must be in (begin;end]")
    }

    # Are all events$tile references really part of the stgrid?
    .events.tile <- factor(events$tile, levels = levels(stgrid$tile))
    if (missingSCellIdx <- match(NA, .events.tile, nomatch = 0L)) {
        stop("the 'events$tile' entry \"", events$tile[missingSCellIdx], "\"",
             " is not a valid level of 'stgrid$tile'")
    }
    events$tile <- .events.tile

    # Calculate time point of removal, when event is definitely no longer infective
    removalTimes <- events$time + events$eps.t

    # Calculate distance matrix of events
    cat("Calculating distance matrix of events...\n")
    eventDists <- as.matrix(dist(eventCoords, method = "euclidean"))
#     diag(eventDists) <- Inf   # infinite distance to oneself (no self-infection), not necessary

    # Map events to corresponding grid cells
    # Also precalculate possible origins of events (other infected individuals)
    cat("Mapping events to 'stgrid' cells and",
        "determining potential event sources...\n")
    gridcellsOfEvents <- integer(nEvents)
    eventSources <- vector(nEvents, mode = "list")
    for (i in seq_len(nEvents)) {
        idx <- gridcellOfEvent(events$time[i], events$tile[i], stgrid)
        if (is.na(idx)) {
            stop("could not find information for time point ", events$time[i],
                 " and tile \"", events$tile[i], "\" in 'stgrid'")
        }
        gridcellsOfEvents[i] <- idx
        eventSources[[i]] <- determineSources(
            i, events$time, removalTimes, eventDists[i,], events$eps.s, events$type, qmatrix
        )
    }

    # Attach endemic covariates from stgrid to events
    cat("Attaching endemic covariates from 'stgrid' to 'events'...\n")
    stgridIgnoreCols <- match(setdiff(obligColsNames_stgrid, "start"), names(stgrid))
    copyCols <- setdiff(seq_along(stgrid), stgridIgnoreCols)
    events@data <- cbind(events@data, stgrid[gridcellsOfEvents, copyCols])

    # Calculate observed infection length = min(T-time, eps.t) for use in log-likelihood
    events$.obsInfLength <- with(events@data, pmin(timeRange[2]-time, eps.t))

    # Attach possible eventSources (infective individuals) to events
    events$.sources <- eventSources

    # Calculate minimal distance of event locations from the polygonal boundary
    cat("Calculating (minimal) distances of the events to the boundary...\n")
    Wgpc <- as(W, "gpc.poly")
    events$.bdist <- bdist(eventCoords, Wgpc)   # this may take a while

    # Construct spatial influence regions around events
    cat("Constructing spatial influence regions around events...\n")
    events$.influenceRegion <- .influenceRegions(events, Wgpc, npoly = nCircle2Poly)

    # Attach some useful attributes
    res <- list(events = events, stgrid = stgrid, W = W, qmatrix = qmatrix)
    class(res) <- c("epidataCS", "list")

    cat("Done.\n\n")
    return(res)
}






######################################################################
# HELPER FUNCTIONS FOR as.epidataCS
######################################################################


### CHECK FUNCTION FOR events ARGUMENT IN as.epidataCS

checkEvents <- function (events, dropTypes = TRUE)
{
    # Check class and spatial dimensions
    stopifnot(inherits(events, "SpatialPointsDataFrame"))
    if (ncol(events@coords) != 2L) {
        stop("only two spatial dimensions are supported")
    }

    # Check existence of type column
    cat("\tChecking 'type' column... ")
    events$type <- if ("type" %in% names(events)) {
                       if (dropTypes) factor(events$type) else as.factor(events$type)
                   } else {
                       cat("Setting 'type' to 1 for all events.")
                       factor(rep.int(1L,nrow(events@coords)))
                     }
    cat("\n")

    # Check obligatory columns
    obligColsIdx <- match(obligColsNames_events, names(events), nomatch = NA_integer_)
    if (any(obligColsMissing <- is.na(obligColsIdx))) {
        stop("missing obligatory columns in 'events': ",
            paste(obligColsNames_events[obligColsMissing], collapse = ", "))
    }

    # Check that influence radii are numeric and positive
    cat("\tChecking 'eps.t' and 'eps.s' columns...\n")
    with(events@data, stopifnot(is.numeric(eps.t), eps.t > 0,
                                is.numeric(eps.s), eps.s > 0))

    # Transform time into a numeric variable
    cat("\tConverting event time into a numeric variable...\n")
    events$time <- as.numeric(events$time)

    # Check event times for ties
    cat("\tChecking event times for ties...\n")
    timeIsDuplicated <- duplicated(events$time)
    if (any(timeIsDuplicated)) {
        duplicatedTimes <- unique(events$time[timeIsDuplicated])
        stop("non-unique event times: concurrent events at time point(s)\n",
             paste(duplicatedTimes, collapse = ", "))
    }

    cat("\tSorting events...\n")
    # Attribute unique IDs to events (running chronologically from 1 to nEvents)
    events$ID <- order(events$time)

    # Make ID column the first column, then obligatory columns then remainders (epidemic covariates)
    IDcolIdx <- match("ID", names(events))
    covarColsIdx <- setdiff(seq_along(events@data), c(IDcolIdx, obligColsIdx))
    events <- events[c(IDcolIdx, obligColsIdx, covarColsIdx)]

    # Sort events chronologically
    events <- events[events$ID,]

    # Done.
    return(events)
}



### CHECK FUNCTION FOR stgrid ARGUMENT IN as.epidataCS

checkstgrid <- function(stgrid)
{
    # Check class
    stopifnot(inherits(stgrid, "data.frame"))

    # Check existence of area column
    cat("\tChecking 'area' column... ")
    stgrid$area <- if ("area" %in% names(stgrid)) {
                     as.numeric(stgrid$area)
                   } else {
                     cat("Setting 'area' to 1 for all grid cells.")
                     1
                   }
    cat("\n")

    # Check obligatory columns
    obligColsIdx <- match(obligColsNames_stgrid, names(stgrid), nomatch = NA_integer_)
    if (any(obligColsMissing <- is.na(obligColsIdx))) {
        stop("missing obligatory columns in 'stgrid': ",
            paste(obligColsNames_stgrid[obligColsMissing], collapse = ", "))
    }

    # Transform tile into a factor variable (also removing unused levels if it was a factor)
    cat("\tConverting 'tile' into a factor variable...\n")
    stgrid$tile <- factor(stgrid$tile)

    # Transform start/stop times into numeric variables
    cat("\tConverting 'start' and 'stop' into numeric variables...\n")
    stgrid$start <- as.numeric(stgrid$start)
    stgrid$stop <- as.numeric(stgrid$stop)

    # Check start/stop consistency
    cat("\tChecking start/stop consisteny...\n")
    histIntervals <- unique(stgrid[c("start", "stop")])
    histIntervals <- histIntervals[order(histIntervals[,1L]),]
    nBlocks <- nrow(histIntervals)
    if (any(histIntervals[,2L] <= histIntervals[,1L])) {
        stop("stop times must be greater than start times")
   }
    startStopCheck <- histIntervals[-1L,1L] != histIntervals[-nBlocks,2L]
    if (startStopCheckIdx <- match(TRUE, startStopCheck, nomatch = 0)) {
        stop("inconsistent start/stop times: time intervals not consecutive ",
             "at stop time ", histIntervals[startStopCheckIdx,2L])
    }

    # Add BLOCK id
    cat("\tChecking if the grid is rectangular (all time-space combinations)...\n")
    if ("BLOCK" %in% names(stgrid)) {
        warning("in data.frame 'stgrid' the column name 'BLOCK' is reserved, ",
                "existing column has been replaced")
    }
    stgrid$BLOCK <- match(stgrid$start, histIntervals[,1L])

    # Check unique BLOCK size
    blocksizes <- table(stgrid$BLOCK)
    if (any(diff(blocksizes) != 0L)) {
        warning("different BLOCK sizes")
    }

    # Make BLOCK column the first column, then obligatory columns, then remainders (endemic covariates)
    cat("\tSorting the grid by time and tile...\n")
    BLOCKcolIdx <- match("BLOCK", names(stgrid))
    covarColsIdx <- setdiff(seq_along(stgrid), c(BLOCKcolIdx, obligColsIdx))
    stgrid <- stgrid[c(BLOCKcolIdx, obligColsIdx, covarColsIdx)]

    # Sort by BLOCK and tile
    stgrid <- stgrid[order(stgrid$BLOCK, stgrid$tile),]

#     # Get row indexes of the blocks' first/last rows
#     beginBlock <- match(seq_len(nBlocks), stgrid[["BLOCK"]])
#     endBlock <- c(beginBlock[-1L]-1L, nrow(stgrid))

    # Done.
    return(stgrid)
}



### CONSTRUCT SPATIAL INFLUENCE REGIONS AROUND EVENTS

# An influenceRegion is an object of class "owin" with origin
# at the event (over which we have to integrate by a cubature rule)
# An attribute "area" gives the area of the influenceRegion.
# If it is actually a circular influence region, then there is an attribute
# "radius" denoting the radius of the influence region.
.influenceRegions <- function (events, Wgpc, npoly) {
    ext <- sqrt(sum(sapply(get.bbox(Wgpc), diff)^2))   # length of the diagonal of the bounding box
    eventCoords <- coordinates(events)
    nEvents <- nrow(eventCoords)
    res <- vector(nEvents, mode = "list")
    for (i in seq_len(nEvents)) {
        eps <- events$eps.s[i]
        center <- eventCoords[i,]
        res[[i]] <- if (eps > ext) {   # influence region is whole region of W
                as.owin(scale(Wgpc, center = center))
            } else {   # influence region is a subset of W
                as.owin(intersectCircle(Wgpc, center, eps, npoly))
            }
        # if influence region actually is a circle of radius eps, attach eps as attribute
        r <- if (eps <= events$.bdist[i]) eps else NULL
        attr(res[[i]], "radius") <- r
        attr(res[[i]], "area") <- if(is.null(r)) area.owin(res[[i]]) else pi*r^2
    }
    attr(res, "nCircle2Poly") <- npoly
    return(res)
}






######################################################################
# S3-methods for 'epidataCS' objects
######################################################################


### UPDATE eps.s, eps.t, qmatrix OR nCircle2Poly IN AN EXISTING epidataCS OBJECT

# all arguments but 'object' are optional, the ... argument is unused
update.epidataCS <- function (object, eps.t, eps.s, qmatrix, nCircle2Poly, ...)
{
    nEvents <- nrow(object$events@data)

    # Check and update eps.t
    if (!missing(eps.t)) {
        stopifnot(is.numeric(eps.t), eps.t > 0)
        object$events$eps.t <- eps.t
    }

    # Initialise indicator of which influenceRegions to update
    ir2update <- logical(nEvents)   # all FALSE

    # Check and update eps.s
    if (!missing(eps.s)) {
        stopifnot(is.numeric(eps.s), eps.s > 0)
        oldeps.s <- object$events$eps.s
        object$events$eps.s <- eps.s
        ir2update <- oldeps.s != object$events$eps.s
    }

    # Check nCircle2Poly
    if (missing(nCircle2Poly)) {
        nCircle2Poly <- attr(object$events$.influenceRegion, "nCircle2Poly")
    } else {
        stopifnot(isScalar(nCircle2Poly))
        nCircle2Poly <- as.integer(nCircle2Poly)
        ir2update <- rep(TRUE, nEvents)
    }

    # Update influenceRegions of events
    if (any(ir2update)) {
        Wgpc <- as(object$W, "gpc.poly")
        object$events$.influenceRegion[ir2update] <-
            .influenceRegions(object$events[ir2update,], Wgpc, nCircle2Poly)
        attr(object$events$.influenceRegion, "nCircle2Poly") <- nCircle2Poly
    }

    # Check qmatrix
    if (!missing(qmatrix)) object$qmatrix <- checkQ(qmatrix, levels(object$events$type))

    #hoehle @ 16 Apr 2011 - bug fix. .obsInfLength was not handled
    # Update length of infection time, i.e. length = min(T-time, eps.t)
    if (!missing(eps.t)) {
      timeRange <- with(object$stgrid, c(start[1], stop[length(stop)]))
      object$events$.obsInfLength <- with(object$events@data, pmin(timeRange[2]-time, eps.t))
    }

    # Update .sources
    if (!missing(eps.t) || !missing(eps.s) || !missing(qmatrix)) {
        eventTimes <- object$events$time
        removalTimes <- eventTimes + object$events$eps.t
        eventDists <- as.matrix(dist(object$events@coords, method = "euclidean"))
        object$events$.sources <- lapply(seq_len(nEvents), function (i) {
            determineSources(i, eventTimes, removalTimes, eventDists[i,],
                             object$events$eps.s, object$events$type, object$qmatrix)
        })
    }

    # Done update.
    return(object)
}



### extract marks of the events

marks.epidataCS <- function (x, ...) {
    markCols <- seq_len(match("BLOCK",names(x$events))-1L)
    as.data.frame(x$events[markCols])
}



### print

# setOldClass(c("epidataCS", "list"))
# setMethod("head", "epidataCS", function (x, n = 6L, ...)
head.epidataCS <- function (x, n = 6L, ...)
{
    visibleCols <- grep("^\\..+", names(x$events@data), invert = TRUE)
    utils:::head.data.frame(x$events[visibleCols], n = n, ...)
}

tail.epidataCS <- function (x, n = 6L, ...)
{
    visibleCols <- grep("^\\..+", names(x$events@data), invert = TRUE)
    utils:::tail.data.frame(x$events[visibleCols], n = n, ...)
}


print.epidataCS <- function (x, n = 6L, digits = getOption("digits"), ...)
{
    nRowsGrid <- nrow(x$stgrid)
    timeRange <- c(x$stgrid$start[1], x$stgrid$stop[nRowsGrid])
    bboxtxt <- paste(apply(bbox(x$W), 1,
        function (int) paste("[", paste(format(int, trim=TRUE, digits=digits), collapse=", "), "]", sep="")
        ), collapse = " x ")
    nBlocks <- x$stgrid$BLOCK[nRowsGrid]
    nTiles <- nlevels(x$stgrid$tile)
    typeNames <- levels(x$events$type)
    nEvents <- nrow(x$events@coords)
    cat("\nHistory of an epidemic\n")
    cat("Observation period:", paste(format(timeRange, trim=TRUE, digits=digits), collapse = " -- "), "\n")
    cat("Observation window (bounding box):", bboxtxt, "\n")
    cat("Spatio-temporal grid (not shown):", nBlocks,
        ngettext(nBlocks, "time block,", "time blocks,"),
        nTiles, ngettext(nTiles, "tile", "tiles"), "\n")
    cat("Types of events:", paste("'",typeNames,"'",sep=""), "\n")
    cat("Overall number of events:", nEvents, "\n\n")
    # 'print.SpatialPointsDataFrame' does not pass its "digits" argument on to 'print.data.frame', hence the use of options()
    odigits <- options(digits=digits)
    print(head(x, n = n), ...)
    options(odigits)
    if (n < nEvents) cat("[....]\n")
    cat("\n")
    invisible(x)
}



### SUMMARY
# the epidemic is summarized by the following returned components:
# timeRange, nEvents, eventTimes, eventCoords, nSources, as well as
# - tile/typetable: number of events per tile/type
# - counter: number of infective individuals as stepfun

summary.epidataCS <- function (object, ...)
{
    coords <- coordinates(object$events)
    times <- object$events$time
    nEvents <- length(times)
    timeRange <- with(object$stgrid, c(start[1], stop[length(stop)]))
    tiles <- object$events$tile
    bbox <- bbox(object$W)
    tileTable <- c(table(tiles))
    types <- object$events$type
    nTypes <- nlevels(types)
    typeTable <- c(table(types))
    nSources <- sapply(object$events$.sources, length)

    removalTimes <- times + object$events$eps.t
    tps <- sort(unique(c(times, removalTimes)))
    nInfectious <- sapply(tps, function(t) sum(times < t & removalTimes >= t))
    counter <- stepfun(tps, c(0,nInfectious), right = TRUE)

    list(timeRange = timeRange, bbox = bbox, nEvents = nEvents, nTypes = nTypes,
         eventTimes = times, eventCoords = coords, eventTypes = types,
         tileTable = tileTable, typeTable = typeTable,
         counter = counter, nSources = nSources)
}



### animate
# spatio-temporal animation, two types:
# time.spacing=NULL: sequential plots regardless of time between events (i.e. only ordering)
# time.spacing=scalar: chronological animation with timer. if time.spacing = NA, then the time step is automatically determined such that ani.options("nmax") snapshots result.
# respects ani.options "interval" and "nmax"

animate.epidataCS <- function (object, interval = c(0,Inf), time.spacing = NULL,
    legend.opts = list(), timer.opts = list(), pch = 15:18,
    col.current = "red", col.I = "#C16E41", col.R = "#B3B3B3", col.influence = "#FEE0D2", ...)
{
    library("animation")
    stopifnot(is.numeric(interval), length(interval) == 2L)
    sleep <- ani.options("interval")
    nmax <- ani.options("nmax")
    s <- summary(object)
    removalTimes <- s$eventTimes + object$events$eps.t
    eventCoordsTypes <- cbind(s$eventCoords, type = s$eventTypes)
    pch <- rep(pch, length.out = s$nTypes)
    typeNames <- names(s$typeTable)
    multitype <- length(typeNames) > 1L

    # set default legend options
    doLegend <- if (is.list(legend.opts)) {
        if (is.null(legend.opts[["x"]])) legend.opts$x <- "topright"
        if (is.null(legend.opts$title))  legend.opts$title <-
            if (multitype) "type" else "state"
        if (is.null(legend.opts$legend)) { legend.opts$legend <-
            if (multitype) typeNames else c("infectious", "removed")
        }
        if (is.null(legend.opts$col)) { legend.opts$col <-
            if (multitype) col.current else c(col.I, col.R)
        }
        if (is.null(legend.opts$pch)) legend.opts$pch <- pch
        TRUE
    } else FALSE

    # set default timer options
    doTimer <- if (is.list(timer.opts)) {
        if (is.null(timer.opts[["x"]]))  timer.opts$x <- "bottomright"
        if (is.null(timer.opts$title))   timer.opts$title <- "time"
        if (is.null(timer.opts$box.lty)) timer.opts$box.lty <- 0
        if (is.null(timer.opts$adj))     timer.opts$adj <- c(0.5,0.5)
        if (is.null(timer.opts$inset))   timer.opts$inset <- 0.01
        if (is.null(timer.opts$bg))      timer.opts$bg <- "white"
        TRUE
    } else FALSE

    # helper function determines multiplicity of rows of a numeric matrix
    # and returns unique rows with appended multiplicity column
    multunique <- function (table) {
        distmat <- as.matrix(dist(table))
        mult <- rowSums(distmat == 0)
        unique(cbind(table, mult))
        #which(upper.tri(distmat, diag=FALSE) & distmat == 0, arr.ind = TRUE)
    }
    # wrapper for 'points' with specific 'cex' for multiplicity
    multpoints <- function (tableCoordsTypes, col) {
        tableMult <- multunique(tableCoordsTypes)
        points(tableMult[,1:2,drop=FALSE], pch = pch[tableMult[,"type"]],
               col = col, cex = sqrt(1.5*tableMult[,"mult"]/pi) * par("cex"))
    }
    # functions returning if events are in status I or R at time t
    I <- function (t) s$eventTimes <= t & removalTimes >= t
    R <- function (t) removalTimes < t

    sequential <- is.null(time.spacing)  # plot observed infections sequentially
    if (!sequential) stopifnot(length(time.spacing) == 1L)
    timeGrid <- if (sequential) s$eventTimes else {
        start <- max(s$timeRange[1], interval[1])
        end <- min(interval[2], s$timeRange[2],
            max(removalTimes) + if (is.na(time.spacing)) 0 else time.spacing)
        if (is.na(time.spacing)) {
            seq(from = start, to = end, length.out = nmax)
        } else {
            tps <- seq(from = start, to = end, by = time.spacing)
            if (length(tps) > nmax) {
                message("Generating only the first ",
                    sQuote("ani.options(\"nmax\")"), " (=", nmax, ") snapshots")
                head(tps, nmax)
            } else tps
        }
    }
    .info <- format.info(timeGrid)
    timerformat <- paste("%", .info[1], ".", .info[2], "f", sep = "")

    # animate
    loopIndex <- if (!sequential) timeGrid else {
        idxs <- which(s$eventTimes >= interval[1] & s$eventTimes <= interval[2])
        if (length(idxs) > nmax) {
            message("Generating only the first ",
                sQuote("ani.options(\"nmax\")"), " (=", nmax, ") events")
            head(idxs, nmax)
        } else idxs
    }
    told <- -Inf
    for(it in loopIndex) {
        t <- if (sequential) s$eventTimes[it] else it
        infectious <- I(t)
        removed <- R(t)
        plot(object$W, ...)
        if (doLegend) do.call(legend, legend.opts)
        if (doTimer) {
            ttxt <- sprintf(timerformat, t)
            do.call(legend, c(list(legend = ttxt), timer.opts))
        }
        if (!is.null(col.influence)) {
            iRids <- which(infectious)
            if (sequential) setdiff(iRids, it)
            for(j in iRids) {
                iR <- shift(object$events@data$.influenceRegion[[j]],
                            vec = s$eventCoords[j,])
                plot(iR, add = TRUE, col = col.influence, border = NA)
            }
        }
        rTable <- eventCoordsTypes[removed,,drop=FALSE]
        if (nrow(rTable) > 0L) multpoints(rTable, col = col.R)
        iTable <- eventCoordsTypes[infectious,,drop=FALSE]
        if (nrow(iTable) > 0L) multpoints(iTable, col = col.I)
        infectiousNew <- if (sequential) it else infectious & !I(told)
        iTableNew <- eventCoordsTypes[infectiousNew,,drop=FALSE]
        if (nrow(iTableNew) > 0L) multpoints(iTableNew, col = col.current)
        told <- t
        Sys.sleep(sleep)
    }
    invisible(NULL)
}






######################################################################
# Transform _twinstim_ epidataCS to _twinSIR_ epidata object
######################################################################

# this only generates a SIS epidemic, i.e. atRiskY is set to 1 immediately after recovery
# length of infectious period is taken from epidataCS$events$eps.t
# fcols are not generated here. these must be generated by a second call to twinSIR's as.epidata with desired f. (for safety)
# tileCentroids is a coordinate matrix whose row names are the tile levels
as.epidata.epidataCS <- function (epidataCS, tileCentroids, eps = 0.001)
{
    ### generate twinSIR's epidata object from stgrid (no events)
    centroidIdx <- match(levels(epidataCS$stgrid$tile), rownames(tileCentroids), nomatch = NA_integer_)
    if (any(is.na(centroidIdx))) {
        stop("some levels of 'epidataCS$stgrid$tile' are not available from 'tileCentroids'")
    }
    centroids <- tileCentroids[centroidIdx,]
    if (any(c("xCent", "yCent") %in% names(epidataCS$stgrid))) {
        stop("'epidataCS$stgrid' already has columns \"xCent\" and \"yCent\"")
    }
    stgrid <- cbind(epidataCS$stgrid,
        atRiskY = 1L, event = 0L, Revent = 0L,
        xCent = centroids[,1], yCent = centroids[,2]
        # relies on ordering of stgrid by first BLOCK, then tile
    )
    names(stgrid)[names(stgrid)=="tile"] <- "id"

    ### now determine "events" with respect to the tiles
    # individual data
    indItimes <- epidataCS$events$time
    indRtimes <- indItimes + epidataCS$events$eps.t
    indInts <- Intervals(cbind(indItimes, indRtimes, deparse.level = 0L))
    indTiles <- epidataCS$events$tile

    # tile data
    tileRows <- tapply(seq_along(indTiles), indTiles, c, simplify = FALSE)
    tileInts <- lapply(tileRows, function (rows) {
        if (length(rows)==0L) { matrix(0,0,2) } else if (length(rows)==1L) {
            as.matrix(indInts[rows])
        } else as.matrix(reduce(indInts[rows]))
    })
    tileNames <- rep(names(tileInts), sapply(tileInts, nrow))
    tileItimes <- unlist(lapply(tileInts, function(ints) ints[,1]), use.names=FALSE)
    tileRtimes <- unlist(lapply(tileInts, function(ints) ints[,2]), use.names=FALSE)

    # there are possibly Rtimes which equal Itimes of other individuals
    # => break ties by considering Rtime shortly before Itime (arbitrary choice)
    while(length(dup <- which(tileRtimes %in% tileItimes)) > 0L) {
        tileRtimes[dup] <- tileRtimes[dup] - eps
    }
    # now there could be duplicated Rtimes... grml (choose another 'eps' in this case)
    if (anyDuplicated(tileRtimes)) {
        stop("breaking ties introduced duplicated Rtimes")
    }

    ### add additional stop times to stgrid for tile infections and recoveries
    requiredStopTimes <- sort(c(tileItimes, tileRtimes))
    class(stgrid) <- c("epidataCS", "data.frame")
    attr(stgrid, "timeRange") <- c(stgrid$start[1], tail(stgrid$stop,1))
    cat("Inserting extra stop times in 'stgrid' (this might take a while)... ")
    evHist <- intersperse(stgrid, requiredStopTimes)
    class(evHist) <- "data.frame"
    ### <- THIS IS THE MOST TIME-CONSUMING PART OF THIS FUNCTION !!!
    cat("Done.\n")

    ### set event, Revent and atRiskY indicators
    tileNamesCodes <- match(tileNames, levels(evHist$id))
    # event indicator (currently in evHist event==0 everywhere)
    idxItimes <- match(tileItimes, evHist$stop) - 1L + tileNamesCodes
    evHist$event[idxItimes] <- 1L
    # Revent indicator (currently in evHist Revent==0 everywhere)
    idxRtimes <- match(tileRtimes, evHist$stop) - 1L + tileNamesCodes  # (may contain NA's if Revent after last stop)
    evHist$Revent[idxRtimes] <- 1L
    # atRiskY indicator
    .atRiskY <- rep.int(1L, nrow(evHist))
    nTiles <- nlevels(evHist$id)
    nBlocks <- tail(evHist$BLOCK, 1)
    stopTimes <- unique(evHist$stop)  # has length nBlocks
    for (i in seq_along(tileItimes)) {
        .Itime <- tileItimes[i]
        .Rtime <- tileRtimes[i]
        .tileCode <- tileNamesCodes[i]
        idxsTileInEpi <- seq(.tileCode, by=nTiles, length.out=nBlocks)
        first0block <- match(.Itime, stopTimes) + 1L
        last0block <- if (.Rtime > stopTimes[nBlocks]) nBlocks else match(.Rtime, stopTimes)
        .atRiskY[idxsTileInEpi[first0block:last0block]] <- 0L
    }
    evHist$atRiskY <- .atRiskY

    ### Return final epidata object of twinSIR-type
    cat("Generating final RLadyBug::epidata object for twinSIR... ")
    epi <- as.epidata(evHist[-grep("BLOCK", names(evHist))],
        id.col="id", start.col="start", stop.col="stop", atRiskY.col="atRiskY",
        event.col="event", Revent.col="Revent", coords.cols=c("xCent","yCent")
    )
    cat("Done.\n")
    epi
}

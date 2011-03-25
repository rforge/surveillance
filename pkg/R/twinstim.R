################################################################################
### Function 'twinstim' performs maximum likelihood inference
### for the additive-multiplicative spatio-temporal intensity model.
### It uses 'nlminb' as the default optimizer (Newton-algorithm).
###
### Author: Sebastian Meyer
### $Date: 2010-11-16 04:08:22 +0100 (Tue, 16 Nov 2010) $
################################################################################

# PARAMS:
# endemic: formula for the exponential (Cox-like multiplicative) endemic component. may contain offsets. If ~0 there will be no endemic component in the model
# epidemic: formula representing the epidemic model for the event-specific covariates determining infectivity. offsets are not implemented. If ~0 there will be no epidemic component in the model.
# siaf, tiaf: spatial/temporal interaction functions. May be NULL (or missing), a list (continous function) or numeric (knots of a step function)
# qmatrix: square indicator matrix (0/1 or TRUE/FALSE) for possible transmission between the event types. will be internally converted to logical. Defaults to the Q matrix specified in data.
# data: epidataCS object
# subset: logical expression indicating events to keep: missing values are taken as false. the expression is evaluated in the context of the data$events@data data.frame
# na.action: how to deal with missing values in 'data$events'. Do not use 'na.pass'. Missing values in the spatio-temporal grid 'data$stgrid' are not accepted.
# optim.args: NULL or an argument list passed to 'optim' containing at least the element 'par', the start values of the parameters in the order par = c(endemic, epidemic, siaf, tiaf). Exceptionally, the 'method' argument may also be "nlminb", in which case the 'nlminb' optimizer is used. This is also the default. If 'optim.args' is NULL then no optimization will be performed but the necessary functions will be returned in a list (similar to 'model = TRUE').
#  nCub - determines the accuracy of the cubature of the 'siaf' function. If siaf$Fcircle is specified, nCub = effRange/eps, where eps is used as pixel width and height in the two-dimensional midpoint rule (see polyCub.midpoint). Thus nCub is the desired number of subdivions of effRange in both dimensions and eps = effRange/nCub. If siaf$Fcircle is missing, nCub equals the above mentioned eps.
# partial: logical indicating if the partial log-likelihood proposed by Diggle et al. (2009) should be used.
# finetune: logical indicating if a second maximisation should be performed with robust Nelder-Mead optim using as starting point the resulting parameters from the first maximisation. Default to TRUE.
# t0: events having occured during (-Inf;t0] are regarded as part of the prehistory H_0 of the process. The time point 't0' must be an element of data$stgrid$start. By default it is the earliest time point of the spatio-temporal grid of 'data'.
# T: only use events that occured up to time T. The time point 'T' must be an element of data$stgrid$stop. By default it is the latest time point of the spatio-temporal grid of 'data'.
# typeSpecificEndemicIntercept: Use type-specific endemic intercepts instead of the global endemic intercept?
# model: logical. If 'TRUE' the result contains an element 'functions' with the log-likelihood function, and optionally the score function and the fisher information function. The environment of those functions equals the evaluation environment of the fitting function, i.e. it is kept in the workspace and the necessary model frames are still available when 'twinstim' has finished. This might be of interest for a posteriori evaluations of the log-likelihood, e.g. for plotting purposes.

twinstim <- function (endemic, epidemic, siaf, tiaf, qmatrix = data$qmatrix,
    data, subset, na.action = na.fail, optim.args, nCub, partial = FALSE,
    finetune = TRUE, t0 = data$stgrid$start[1], T = tail(data$stgrid$stop,1),
    typeSpecificEndemicIntercept = FALSE, model = FALSE, cumCIF = TRUE)
{

    ####################
    ### Preparations ###
    ####################

    ptm <- proc.time()[[3]]
    cl <- match.call()
    partial <- as.logical(partial)
    finetune <- if (partial) FALSE else as.logical(finetune)
    typeSpecificEndemicIntercept <- as.logical(typeSpecificEndemicIntercept)

    # Clean the environment when exiting the function
    on.exit(suppressWarnings(rm(cl, data, eventsData, finetune, fisherinfo, fit, functions,
        globalEndemicIntercept, inmfe, ll, negll, loglik, mfe, mfhEvents, mfhGrid,
        my.na.action, na.action, namesOptimUser, namesOptimArgs, nlminbRes, nmRes, optim.args,
        optimValid, optimControl, partial, partialloglik, ptm, qmatrix, res, sc, negsc, score,
        subset, typeSpecificEndemicIntercept, useScore, inherits = FALSE)))


    ### Verify that 'data' inherits from "epidataCS"

    if (!inherits(data, "epidataCS")) {
        stop("'data' must inherit from class \"epidataCS\"")
    }


    ### Check time range

    if (!isScalar(t0) || !isScalar(T)) {
        stop("endpoints 't0' and 'T' must be single numbers")
    }
    if (T <= t0) {
        stop("'T' must be greater than 't0'")
    }
    if (!t0 %in% data$stgrid$start) {
        justBeforet0 <- match(TRUE, data$stgrid$start > t0) - 1L
        # if 't0' is beyond the time range covered by 'data$stgrid'
        if (is.na(justBeforet0)) justBeforet0 <- length(data$stgrid$start)   # t0 was too big
        if (justBeforet0 == 0L) justBeforet0 <- 1L   # t0 was too small
        t0 <- data$stgrid$start[justBeforet0]
        message("replaced 't0' by the value ", t0,
                " (must be a 'start' time of 'data$stgrid')")
    }
    if (!T %in% data$stgrid$stop) {
        justAfterT <- match(TRUE, data$stgrid$stop > T)
        # if 'T' is beyond the time range covered by 'data$stgrid'
        if (is.na(justAfterT)) justAfterT <- length(data$stgrid$stop)   # T was too big
        T <- data$stgrid$stop[justAfterT]
        message("replaced 'T' by the value ", T,
                " (must be a 'stop' time of 'data$stgrid')")
    }
    #timeRange <- c(t0, T)


    ### Subset events

    eventsData <- if (missing(subset)) data$events@data else {
        subsetcall <- call("subset.data.frame",
            x = quote(data$events@data), subset = cl$subset, drop = FALSE)
        eval(subsetcall)
    }






    #############################################################
    ### Build up a model.frame for both components separately ###
    #############################################################



    ##########################
    ### epidemic component ###
    ##########################


    ### Parse epidemic formula

    if (missing(epidemic)) {
        epidemic <- ~ 0
    } else {
        environment(epidemic) <- environment()
        # so that t0 and T are found in the subset expressions below
    }
    epidemic <- terms(epidemic, data = eventsData, keep.order = TRUE)
    if (!is.null(attr(epidemic, "offset"))) {
        warning("offsets are not implemented for the 'epidemic' component")
    }


    ### Generate model frame

    # na.action mod such that for simulated epidataCS, where events of the
    # prehistory have missing 'BLOCK' indexes, those NA's do not matter.
    # ok because actually, 'eventBlocks' are only used in the partial likelihood
    # and there only eventBlocks[includes] is used (i.e. no prehistory events)
    my.na.action <- function (object, ...) {
        prehistevents <- object[object[["(time)"]] <= t0, "(ID)"]
        origprehistblocks <- object[match(prehistevents,object[["(ID)"]]), "(BLOCK)"]
        object[object[["(ID)"]] %in% prehistevents, "(BLOCK)"] <- 0L
        xx <- na.action(object, ...)
        xx[match(prehistevents,xx[["(ID)"]],nomatch=0L), "(BLOCK)"] <-
            origprehistblocks[prehistevents %in% xx[["(ID)"]]]
        xx
    }

    mfe <- model.frame(epidemic, data = eventsData,
                       subset = time + eps.t > t0 & time <= T,
# here we can have some additional rows (individuals) compared to mfhEvents, which is established below!
# Namely those with time in (t0-eps.t; t0], i.e. still infective individuals, which are part of the prehistory of the process
                       na.action = my.na.action,
# since R 2.10.0 patched also works with epidemic = ~1 and na.action=na.fail (see PR#14066)
                       drop.unused.levels = FALSE,
                       ID = ID, time = time, tile = tile, type = type,
                       eps.t = eps.t, eps.s = eps.s, BLOCK = BLOCK,
                       obsInfLength = .obsInfLength, bdist = .bdist)


    ### Extract essential information from model frame

    # inmfe=ID=rowindex(data$events@data) is necessary for subsetting
    # influenceRegion (list object not compatible with model.frame), coordinates and mfhEvents
    inmfe <- mfe[["(ID)"]]     # I don't use model.extract because it returns named vectors
    N <- length(inmfe)   # mfe also contains events of the prehistory
    eventTimes <- mfe[["(time)"]]
    # Indicate events after t0, which are actually part of the process
    # (events in (-Inf;t0] only contribute in sum over infected individuals)
    includes <- which(eventTimes > t0)   # this indexes mfe!
    Nin <- length(includes)
    if (Nin == 0L) {
        stop("none of the ", nrow(data$events@data), " supplied ",
             "events is in the model (check 'subset', 't0' and 'T')")
    }
    eventBlocks <- mfe[["(BLOCK)"]]   # only necessary for partial log-likelihood
    eventTypes <- factor(mfe[["(type)"]])   # drop unused levels
    typeNames <- levels(eventTypes)
    nTypes <- length(typeNames)
    if (nTypes > 1L) cat("marked point pattern of", nTypes, "types\n")
    qmatrix <- checkQ(qmatrix, typeNames)
    # we only need the integer codes for the calculations
    eventTypes <- as.integer(eventTypes)


    ### Generate model matrix

    mme <- model.matrix(epidemic, mfe)
    q <- ncol(mme)
    hase <- q > 0L


    ### Extract further model components (only if q > 0)

    if (hase) {
        eps.t <- mfe[["(eps.t)"]]
        removalTimes <- eventTimes + eps.t
        eps.s <- mfe[["(eps.s)"]]
        bdist <- mfe[["(bdist)"]]
        gIntUpper <- mfe[["(obsInfLength)"]]
        gIntLower <- pmax(0, t0-eventTimes)
# stopifnot(gIntLower < gIntUpper)
        eventCoords <- coordinates(data$events)[inmfe,,drop=FALSE]
        eventDists <- as.matrix(dist(eventCoords, method = "euclidean"))
#         diag(eventDists) <- Inf   # infinite distance to oneself (no self-infection), not necessary
        influenceRegion <- data$events@data$.influenceRegion[inmfe]
        iRareas <- sapply(influenceRegion, attr, "area")
        # determine possible event sources (need to re-do this because
        # subsetting has crashed old row indexes from the epidataCS object)
        # actually only eventSources of includes are needed
        eventSources <- lapply(1:N, function (i) {
            determineSources(i, eventTimes, removalTimes, eventDists[i,], eps.s, eventTypes, qmatrix)
        })
        # calculate sum_{k=1}^K q_{kappa_j,k} for all j = 1:N
        qSum <- rowSums(qmatrix)[eventTypes]   # N-vector
    } else message("no epidemic component in model")



    #########################
    ### endemic component ###
    #########################


    ### Parse endemic formula

    if (missing(endemic)) {
        endemic <- ~ 0
    } else {
        environment(endemic) <- environment()
        # so that t0 and T are found in the subset expressions below
    }
    endemic <- terms(endemic, data = data$stgrid, keep.order = TRUE)

    globalEndemicIntercept <- if (typeSpecificEndemicIntercept) {
            attr(endemic, "intercept") <- 1L   # we need this to ensure that we have correct contrasts
            FALSE
        } else attr(endemic, "intercept") == 1L

    nbeta0 <- globalEndemicIntercept + typeSpecificEndemicIntercept * nTypes


    ### Generate endemic model frame and model matrix on event data

    mfhEvents <- model.frame(endemic, data = eventsData,
                             subset = time > t0 & time <= T & ID %in% inmfe,
                             na.action = na.fail,
                             # since R 2.10.0 patched also works with endemic = ~1 (see PR#14066)
                             drop.unused.levels = FALSE)
    mmhEvents <- model.matrix(endemic, mfhEvents)
    # exclude intercept from endemic model matrix below, will be treated separately
    if (nbeta0 > 0) mmhEvents <- mmhEvents[,-1,drop=FALSE]
# stopifnot(nrow(mmhEvents) == Nin)
    p <- ncol(mmhEvents)
    hash <- (nbeta0+p) > 0L


    ### Generate model frame and model matrix on grid data (only if p > 0)

    if (hash) {
        offsetEvents <- model.offset(mfhEvents)
        mfhGrid <- model.frame(endemic, data = data$stgrid,
                               subset = start >= t0 & stop <= T,
                               na.action = na.fail,
                               # since R 2.10.0 patched also works with endemic = ~1 (see PR#14066)
                               drop.unused.levels = FALSE,
                               BLOCK = BLOCK, tile = tile, dt = stop-start, ds = area)
                               # 'tile' is actually redundant here, but can help as reference when debugging
        gridBlocks <- mfhGrid[["(BLOCK)"]]
        mmhGrid <- model.matrix(endemic, mfhGrid)

        # exclude intercept from endemic model matrix below, will be treated separately
        if (nbeta0 > 0) mmhGrid <- mmhGrid[,-1,drop=FALSE]
        # Extract endemic model components
        offsetGrid <- model.offset(mfhGrid)
        dt <- mfhGrid[["(dt)"]]
        ds <- mfhGrid[["(ds)"]]
    } else message("no endemic component in model")


    ### Check that there is at least one parameter

    if (!hash && !hase) {
        stop("nothing to do: neither endemic nor epidemic parts were specified")
    }






    #############################
    ### Interaction functions ###
    #############################

    if (hase) {

        ### Check interaction functions
        siaf <- do.call(".parseiaf", args = alist(siaf))
        tiaf <- do.call(".parseiaf", args = alist(tiaf))

        ### Spatially constant interaction siaf(s) = 1
        constantsiaf <- is.null(siaf) || isTRUE(attr(siaf, "constant"))
        if (constantsiaf) {
            siaf <- siaf.constant()
        } else attr(siaf, "constant") <- FALSE
        nsiafpars <- siaf$npars

        ### Temporally constant interaction tiaf(t) = 1
        constanttiaf <- is.null(tiaf) || isTRUE(attr(tiaf, "constant"))
        if (constanttiaf) {
            tiaf <- tiaf.constant()
        } else attr(tiaf, "constant") <- FALSE
        ntiafpars <- tiaf$npars

        ### Define function that integrates the two-dimensional 'siaf' function
        ### over the influence regions of the events
        .siafInt <-
            if (constantsiaf) {
                function (siafpars) iRareas
            } else if (is.null(siaf$Fcircle)) { # if siaf$Fcircle is not available
                function (siafpars) {
                    siafInts <- sapply(1:N, function (i) {
                        polyCub.midpoint(influenceRegion[[i]], siaf$f, siafpars, eventTypes[i], eps = nCub)
                    })
                    siafInts
                }
            } else { # fast integration over circular domains !
                function (siafpars) {
                    # Compute computationally effective range of the 'siaf' function
                    # for the current 'siafpars' for each event (type)
                    effRangeTypes <- rep(siaf$effRange(siafpars),length.out=nTypes)
                    effRanges <- effRangeTypes[eventTypes]   # N-vector
                    # automatic choice of h (pixel spacing in image)
                    hs <- effRanges / nCub   # could e.g. equal sigma
                    # Compute the integral of 'siaf' over each influence region
                    siafInts <- numeric(N)
                    for(i in 1:N) {
                        eps <- eps.s[i]
                        bdisti <- bdist[i]
                        effRange <- effRanges[i]
                        siafInts[i] <- if (effRange <= bdisti) {
                                # effective region ("6 sigma") completely inside W
                                siaf$Fcircle(min(eps,effRange), siafpars, eventTypes[i])
                            } else if (eps <= bdisti) {
                                # influence region is completely inside W
                                siaf$Fcircle(eps, siafpars, eventTypes[i])
                            } else {
                                # integrate over polygonal influence region
                                polyCub.midpoint(influenceRegion[[i]], siaf$f,
                                    siafpars, eventTypes[i], eps = hs[i])
                            }
                    }
                    siafInts
                }
            }

        ### Check nCub
        if (!constantsiaf) {
            nCub <- as.integer(nCub)
            if (any(nCub <= 0L)) {
                stop("'nCub' must be positive")
            }
        }

    } else {
        siaf <- tiaf <- NULL
        nsiafpars <- ntiafpars <- 0L
    }

    hassiafpars <- nsiafpars > 0L
    hastiafpars <- ntiafpars > 0L






    ############################################################################
    ### Log-likelihood function, score function, expected Fisher information ###
    ############################################################################


    ### Total number of parameters (= length of 'theta')

    npars <- nbeta0 + p + q + nsiafpars + ntiafpars


    ########################
    ### Helper functions ###
    ########################

    # REMINDER:
    #  theta - parameter vector c(beta, gamma, siafpars, tiafpars), where
    #    beta    - parameters of the endemic component exp(offset + eta_h(t,s))
    #    gamma   - parameters of the epidemic term exp(eta_e(t,s))
    #    siafpars- parameters of the epidemic spatial interaction function
    #    tiafpars- parameters of the epidemic temporal interaction function
    #  mmh[Events/Grid] - model matrix related to beta, i.e the endemic component,
    #                     either for events only or for the whole spatio-temporal grid
    #  offset[Events/Grid] - offset vector related to the endemic component (can be NULL),
    #                        either for events only or for the whole spatio-temporal grid
    #  dt, ds - columns of the spatio-temporal grid (dt = stop-start, ds = area)
    #  mme - model matrix related to gamma in the epidemic component
    #  siaf, tiaf - spatial/temporal interaction function (NULL, list or numeric)
    #  eventTimes, eventCoords, eventSources, gIntLower, gIntUpper, influenceRegion -
    #     columns of the events data frame


    if (hash)
    {
        ### Calculates the endemic component
        ### h(t_i,s_i,kappa_i) = exp(offset_i + beta_{0,kappa_i} + eta_h(t_i,s_i))

        .hEvents <- function (beta0, beta) {}
        body(.hEvents) <- as.call(c(as.name("{"),
            if (p > 0L) {
                expression(eta <- drop(mmhEvents %*% beta))
            } else {
                expression(eta <- numeric(Nin))
            },
            if (nbeta0 == 1L) {
                expression(eta <- beta0 + eta)   # global intercept
            } else if (nbeta0 > 1L) {
                expression(eta <- beta0[eventTypes] + eta)   # type-specific intercept
            },
            if (!is.null(offsetEvents)) expression(eta <- offsetEvents + eta),
            expression(exp(eta))
        ))


        ### Integral of the endemic component over [0;uppert] x W

        .hIntTW <- function (beta, score = matrix(1,nrow(mmhGrid),1L), uppert = NULL) {}
        body(.hIntTW) <- as.call(c(as.name("{"),
            expression(
                subtimeidx <- if (!is.null(uppert) && isScalar(uppert) && t0 <= uppert && uppert < T) {
                    if (uppert == t0) return(0)
                    idx <- match(TRUE, data$stgrid$stop >= uppert)
                    firstBlockBeyondUpper <- data$stgrid$BLOCK[idx]
                    newdt <- uppert - data$stgrid$start[idx]
                    dt[gridBlocks == firstBlockBeyondUpper] <- newdt
                    gridBlocks <= firstBlockBeyondUpper
                } else NULL
            ),
            if (p > 0L) {
                expression(eta <- drop(mmhGrid %*% beta))
            } else {
                expression(eta <- numeric(nrow(mmhGrid)))
            },
            if (!is.null(offsetGrid)) expression(eta <- offsetGrid + eta),
            expression(sumterms <- score * (exp(eta)*ds*dt)),
            expression(if (is.null(subtimeidx)) colSums(sumterms) else colSums(sumterms[subtimeidx,,drop=FALSE]))
        ))
    }

    if (hase)
    {
        ### Calculates the epidemic component for all events

        .eEvents <- function (gammapred, siafpars, tiafpars,
            ncolsRes = 1L, score = matrix(1,N,ncolsRes), f = siaf$f, g = tiaf$g)
            # second line arguments are for score functions with defaults for loglik
        {
            e <- matrix(0, N, ncolsRes)
            for (i in includes) {
                sources <- eventSources[[i]]
                nsources <- length(sources)
                e[i,] <- if (nsources == 0L) 0 else {
                    scoresources <- score[sources,,drop=FALSE]
                    predsources <- gammapred[sources]
                    repi <- rep.int(i, nsources)
                    sdiff <- eventCoords[repi,,drop=FALSE] - eventCoords[sources,,drop=FALSE]
                    fsources <- f(sdiff, siafpars, eventTypes[sources])
                    tdiff <- eventTimes[repi] - eventTimes[sources]
                    gsources <- g(tdiff, tiafpars, eventTypes[sources])
        # if(length(predsources) != NROW(fsources) || NROW(fsources) != NROW(gsources)) browser()
                    colSums(scoresources * predsources * fsources * gsources)
                }
            }
            e[includes,,drop=nargs()==3L]   # drop = TRUE for loglik
        }
    }


    ### Calculates the two compontens of the integrated intensity function over [0;uppert] x W x K

    heIntTWK <- function (beta0, beta, gammapred, siafpars, tiafpars, uppert = NULL) {}
    body(heIntTWK) <- as.call(c(as.name("{"),
        if (hash) { # endemic component
            expression({
                hIntTW <- .hIntTW(beta, uppert = uppert)
                fact <- if (nbeta0 > 1L) sum(exp(beta0)) else if (nbeta0 == 1L) nTypes*exp(unname(beta0)) else nTypes
                hInt <- fact * hIntTW
            })
        } else { expression(hInt <- 0) },
        if (hase) { # epidemic component
            expression({
                siafInt <- .siafInt(siafpars) # N-vector
                subtimeidx <- if (!is.null(uppert) && isScalar(uppert) && t0 <= uppert && uppert < T) {
                    gIntUpper <- pmin(uppert-eventTimes, eps.t)
                    eventTimes < uppert
                } else rep(TRUE, N)
                tiafIntUpper <- tiaf$G(gIntUpper[subtimeidx], tiafpars, eventTypes[subtimeidx])
                tiafIntLower <- tiaf$G(gIntLower[subtimeidx], tiafpars, eventTypes[subtimeidx])
                tiafInt <- tiafIntUpper - tiafIntLower
                eInt <- sum(qSum[subtimeidx] * gammapred[subtimeidx] * siafInt[subtimeidx] * tiafInt)
            })
        } else expression(eInt <- 0),
        expression(c(hInt, eInt))
    ))


    ### Calculates the log-likelihood

    loglik <- function (theta)
    {
    # if(any(!is.finite(theta))) browser()
        # Extract parameters from theta
        beta0    <- theta[seq_len(nbeta0)]
        beta     <- theta[nbeta0+seq_len(p)]
        gamma    <- theta[nbeta0+p+seq_len(q)]
        siafpars <- theta[nbeta0+p+q+seq_len(nsiafpars)]
        tiafpars <- theta[nbeta0+p+q+nsiafpars+seq_len(ntiafpars)]

        # check validity of siafpars and tiafpars
        if (hassiafpars && !siaf$validpars(siafpars)) {
            if (optimArgs$control$trace > 0L)
                cat("(invalid 'siafpars' in loglik)\n")
            return(-Inf)
        }
        if (hastiafpars && !tiaf$validpars(tiafpars)) {
            if (optimArgs$control$trace > 0L)
                cat("(invalid 'tiafpars' in loglik)\n")
            return(-Inf)
        }

        # dN part of the log-likelihood
        hEvents <- if (hash) .hEvents(beta0, beta) else 0
        eEvents <- if (hase) {
                gammapred <- drop(exp(mme %*% gamma)) # N-vector
                .eEvents(gammapred, siafpars, tiafpars) # Nin-vector! (only 'includes' here)
            } else 0
        lambdaEvents <- hEvents + eEvents  # Nin-vector
        llEvents <- sum(log(lambdaEvents))
        # here one might have got -Inf values in case of 0-intensity at an event time

        # lambda integral of the log-likelihood
        heInt <- heIntTWK(beta0, beta, gammapred, siafpars, tiafpars)   # !hase => missing(gammapred), but lazy evaluation omits an error in this case because heIntTWK doesn't ask for gammapred
        llInt <- sum(heInt)

        # Return the log-likelihood
        ll <- llEvents - llInt
        ll
    }


    ### Calculates the score vector

    score <- function (theta)
    {
        # Extract parameters from theta
        beta0    <- theta[seq_len(nbeta0)]
        beta     <- theta[nbeta0+seq_len(p)]
        gamma    <- theta[nbeta0+p+seq_len(q)]
        siafpars <- theta[nbeta0+p+q+seq_len(nsiafpars)]
        tiafpars <- theta[nbeta0+p+q+nsiafpars+seq_len(ntiafpars)]

        if (hase) {
            gammapred <- drop(exp(mme %*% gamma))  # N-vector
            hEvents <- if (hash) .hEvents(beta0, beta) else 0
            eEvents <- .eEvents(gammapred, siafpars, tiafpars) # Nin-vector! (only 'includes' here)
            lambdaEvents <- hEvents + eEvents  # Nin-vector
            siafInt <- .siafInt(siafpars) # N-vector
            tiafIntUpper <- tiaf$G(gIntUpper, tiafpars, eventTypes)
            tiafIntLower <- tiaf$G(gIntLower, tiafpars, eventTypes)
            tiafInt <- tiafIntUpper - tiafIntLower   # N-vector
        }

        # score vector for beta
        hScore <- if (hash)
        {
            score_beta0 <- if (nbeta0 > 1L) local({ # type-specific intercepts
                ind <- sapply(1:nTypes, function (type) eventTypes == type) # logical NxnTypes matrix
                sEvents <- if (hase) {
                        ind * hEvents / lambdaEvents
                    } else ind
                sEventsSum <- colSums(sEvents)
                sInt <- exp(beta0) * .hIntTW(beta)
                sEventsSum - sInt
            }) else if (nbeta0 == 1L) local({ # global intercept
                sEvents <- if (hase) {
                        hEvents / lambdaEvents
                    } else rep.int(1, Nin)
                sEventsSum <- sum(sEvents)
                sInt <- nTypes*exp(beta0) * .hIntTW(beta)
                sEventsSum - sInt
            }) else numeric(0)

            score_beta <- if (p > 0L) local({
                sEvents <- if (hase) {
                        mmhEvents * hEvents / lambdaEvents
                    } else mmhEvents
                sEventsSum <- colSums(sEvents)
                fact <- if (nbeta0 > 1L) sum(exp(beta0)) else if (nbeta0 == 1L) nTypes*exp(beta0) else nTypes
                sInt <- fact * .hIntTW(beta, mmhGrid)
                sEventsSum - sInt
            }) else numeric(0)

            c(score_beta0, score_beta)
        } else numeric(0)

        # score vector for gamma, siafpars and tiafpars
        eScore <- if (hase)
        {
            score_gamma <- local({
                nom <- .eEvents(gammapred, siafpars, tiafpars, ncolsRes=q, score = mme)  # Ninxq matrix
                sEventsSum <- colSums(nom / lambdaEvents)
                sInt <- colSums(mme * (qSum * gammapred * siafInt * tiafInt))
                sEventsSum - sInt
            })

            score_siafpars <- if (hassiafpars) local({
                nom <- .eEvents(gammapred, siafpars, tiafpars, ncolsRes=nsiafpars, f=siaf$deriv)  # Ninxnsiafpars matrix
                sEventsSum <- colSums(nom / lambdaEvents)
                epsTypes <- if (!is.null(siaf$effRange)) {
                        siaf$effRange(siafpars) / nCub
                    } else {
                        2 * min(eps.s) / spatstat.options("npixel")
                    }
                epsTypes <- rep(epsTypes, length.out = nTypes)
                derivInt <- sapply(1:nsiafpars, function (paridx) {
                    sapply(1:N, function (i) {
                        polyCub.midpoint(influenceRegion[[i]], function (s) {
                        siaf$deriv(s, siafpars, eventTypes[i])[,paridx,drop=TRUE]
                        }, eps = epsTypes[eventTypes[i]])
                    })
                })  # Nxnsiafpars matrix
                sInt <- colSums(derivInt * (qSum * gammapred * tiafInt))
                sEventsSum - sInt
            }) else numeric(0)

            score_tiafpars <- if (hastiafpars) local({
                nom <- .eEvents(gammapred, siafpars, tiafpars, ncolsRes=ntiafpars, g=tiaf$deriv)  # Ninxntiafpars matrix
                sEventsSum <- colSums(nom / lambdaEvents)
                derivIntUpper <- tiaf$Deriv(gIntUpper, tiafpars, eventTypes)
                derivIntLower <- tiaf$Deriv(gIntLower, tiafpars, eventTypes)
                derivInt <- derivIntUpper - derivIntLower
                sInt <- colSums(derivInt * (qSum * gammapred * siafInt))
                sEventsSum - sInt
            }) else numeric(0)

            c(score_gamma, score_siafpars, score_tiafpars)
        } else numeric(0)

        # return the score vector
        # cat("\t",c(hScore,eScore),"\n")
        c(hScore, eScore)
    }


    ### Estimates the expected Fisher information matrix
    ### by the "optional variation process" (Martinussen & Scheike, p. 64),
    ### or see Rathbun (1996, equation (4.7))

    fisherinfo <- function (theta)
    {
        # Extract parameters from theta
        beta0    <- theta[seq_len(nbeta0)]
        beta     <- theta[nbeta0+seq_len(p)]
        gamma    <- theta[nbeta0+p+seq_len(q)]
        siafpars <- theta[nbeta0+p+q+seq_len(nsiafpars)]
        tiafpars <- theta[nbeta0+p+q+nsiafpars+seq_len(ntiafpars)]

        # only events (intdN) part of the score function needed
        zeromatrix <- matrix(0, Nin, 0)

        if (hase) {
            gammapred <- drop(exp(mme %*% gamma))  # N-vector
            hEvents <- if (hash) .hEvents(beta0, beta) else 0
            eEvents <- .eEvents(gammapred, siafpars, tiafpars) # Nin-vector! (only 'includes' here)
            lambdaEvents <- hEvents + eEvents  # Nin-vector
        }

        # for beta
        hScoreEvents <- if (hash) {
            scoreEvents_beta0 <- if (nbeta0 > 1L) local({ # type-specific intercepts
                ind <- sapply(1:nTypes, function (type) eventTypes == type) # logical NxnTypes matrix
                if (hase) {
                    ind * hEvents / lambdaEvents
                } else ind
            }) else if (nbeta0 == 1L) { # global intercept
                if (hase) {
                    hEvents / lambdaEvents
                } else matrix(1, Nin, 1L)
            } else zeromatrix

            scoreEvents_beta <- if (p > 0L) {
                if (hase) {
                    mmhEvents * hEvents / lambdaEvents
                } else mmhEvents   # Ninxp matrix
            } else zeromatrix

            cbind(scoreEvents_beta0, scoreEvents_beta)
        } else zeromatrix

        # for gamma, siafpars and tiafpars
        eScoreEvents <- if (hase)
        {
            scoreEvents_gamma_nom <-
                .eEvents(gammapred, siafpars, tiafpars, ncolsRes = q, score = mme)  # Ninxq matrix

            scoreEvents_siafpars_nom <- if (hassiafpars) {
                .eEvents(gammapred, siafpars, tiafpars, ncolsRes = nsiafpars, f = siaf$deriv)  # Ninxnsiafpars matrix
            } else zeromatrix

            scoreEvents_tiafpars_nom <- if (hastiafpars) {
                .eEvents(gammapred, siafpars, tiafpars, ncolsRes = ntiafpars, g = tiaf$deriv)  # Ninxntiafpars matrix
            } else zeromatrix

            eScoreEvents_nom <- cbind(scoreEvents_gamma_nom, scoreEvents_siafpars_nom, scoreEvents_tiafpars_nom)
            eScoreEvents_nom / lambdaEvents
        } else zeromatrix

        scoreEvents <- cbind(hScoreEvents, eScoreEvents)

        # Build the optional variation process (Martinussen & Scheike, p64)
        info <- matrix(0, nrow = npars, ncol = npars,
                    dimnames = list(names(theta), names(theta)))
        for (i in 1:Nin) {
            x <- scoreEvents[i,,drop=FALSE]  # single-ROW matrix
            info <- info + crossprod(x) # t(x) %*% x
        }

        # Return the estimated Fisher information matrix
        info
    }


    ### Calculates the partial log-likelihood for continuous space
    ### (Diggle et al., 2009)

    partialloglik <- function (theta)
    {
        # Extract parameters from theta
        beta0    <- theta[seq_len(nbeta0)]
        beta     <- theta[nbeta0+seq_len(p)]
        gamma    <- theta[nbeta0+p+seq_len(q)]
        siafpars <- theta[nbeta0+p+q+seq_len(nsiafpars)]
        tiafpars <- theta[nbeta0+p+q+nsiafpars+seq_len(ntiafpars)]

        # check validity of siafpars and tiafpars
        if (hassiafpars && !siaf$validpars(siafpars)) {
            if (optimArgs$control$trace > 0L) cat("(invalid 'siafpars' in loglik)\n")
            return(-Inf)
        }
        if (hastiafpars && !tiaf$validpars(tiafpars)) {
            if (optimArgs$control$trace > 0L) cat("(invalid 'tiafpars' in loglik)\n")
            return(-Inf)
        }

        # calculcate the observed intensities
        hEvents <- if (hash) .hEvents(beta0, beta) else 0
        eEvents <- if (hase) {
                gammapred <- drop(exp(mme %*% gamma))  # N-vector
                .eEvents(gammapred, siafpars, tiafpars)  # Nin-vector! (only 'includes' here)
            } else 0
        lambdaEvents <- hEvents + eEvents  # Nin-vector

        # calculate integral of lambda(t_i, s, kappa) over at-risk set = (observation region x types)
        hInts <- if (hash) { # endemic component
                etahGrid <- if (p > 0L) drop(mmhGrid %*% beta) else numeric(nrow(mmhGrid))
                if (!is.null(offsetGrid)) etahGrid <- offsetGrid + etahGrid
                hGrid <- exp(etahGrid)
                # integral over W and types for each time block in mfhGrid
                fact <- if (nbeta0 > 1L) sum(exp(beta0)) else if (nbeta0 == 1L) nTypes*exp(beta0) else nTypes
                hInt_blocks <- fact * tapply(hGrid*ds, gridBlocks, sum, simplify = TRUE)
                .idx <- match(eventBlocks[includes], names(hInt_blocks))
                unname(hInt_blocks[.idx])   # Nin-vector
            } else 0
        eInts <- if (hase) { # epidemic component
                siafInt <- .siafInt(siafpars) # N-vector
                gs <- gammapred * siafInt # N-vector
                sapply(includes, function (i) {
                    timeSources <- determineSources(i, eventTimes, removalTimes,
                        0, Inf, NULL)
                    nSources <- length(timeSources)
                    if (nSources == 0L) 0 else {
                        repi <- rep.int(i, nSources)
                        tdiff <- eventTimes[repi] - eventTimes[timeSources]
                        gsources <- tiaf$g(tdiff, tiafpars, eventTypes[timeSources])
                        sum(qSum[timeSources] * gs[timeSources] * gsources)
                    }
                })   # Nin-vector
            } else 0
        lambdaEventsIntW <- hInts + eInts   # Nin-vector

        # Calculate and return the partial log-likelihood
        p <- lambdaEvents / lambdaEventsIntW   # Nin-vector
        sum(log(p))
    }






    ####################
    ### Optimization ###
    ####################


    ### Choose log-likelihood function

    ll <- if (partial) partialloglik else loglik
    negll <- function (theta) -ll(theta)


    ### Can we use the score function during optim?

    useScore <- if (partial) FALSE else if (hase) {
        (!hassiafpars | !is.null(siaf$deriv)) &
        (!hastiafpars | (!is.null(tiaf$deriv)) & !is.null(tiaf$Deriv))
    } else TRUE
    sc <- if (useScore) score else NULL
    negsc <- if (useScore) function (theta) -sc(theta) else NULL

    functions <- list(ll=ll, sc=sc, fi=if (useScore) fisherinfo else NULL)


    ### Check that optim.args is a list or NULL

    if (missing(optim.args) || (!is.list(optim.args) && !is.null(optim.args))) {
        stop("'optim.args' must be a list or NULL")
    }


    ### Is optimisation requested?

    if (is.null(optim.args)) {
        setting <- functions
        on.exit(rm(setting), add = TRUE)
        # Append model information
        setting$npars <- c(nbeta0 = nbeta0, p = p,
                           q = q, nsiafpars = nsiafpars, ntiafpars = ntiafpars)
        setting$qmatrix <- qmatrix   # -> information about nTypes and typeNames
        setting$formula <- list(endemic = formula(endemic), epidemic = formula(epidemic),
                            siaf = siaf, tiaf = tiaf)
        setting$call <- cl
        # Return settings
        message("optimization skipped (returning functions in data environment)")
        return(setting)
    }


    ### Check start value for theta

    if (is.null(optim.args[["par"]])) {
        stop("start values of parameters must be specified as 'optim.args$par'")
    }
    if (!is.vector(optim.args$par, mode="numeric")) {
        stop("'optim.args$par' must be a numeric vector")
    }
    if (length(optim.args$par) != npars) {
        stop(gettextf(paste("'optim.args$par' (%d) does not have the same",
             "length as the number of unknown parameters (%d)"),
             length(optim.args$par), npars))
    }


    ### Set names for theta

    names(optim.args$par) <- c(
        if (nbeta0 > 1L) {
            paste("h.type",typeNames,sep="")
        } else if (nbeta0 == 1L) "h.(Intercept)",
        if (p > 0L) paste("h", colnames(mmhEvents), sep = "."),
        if (hase) paste("e", colnames(mme), sep = "."),
        if (hassiafpars) paste("e.siaf",1:nsiafpars,sep="."),
        if (hastiafpars) paste("e.tiaf",1:ntiafpars,sep=".")
    )


    ### Configure the optim procedure (check optim.args)

    # default control arguments
    optimControl <- list(trace = 1L, REPORT = 5L)
    # merge with user control arguments
    optimControl[names(optim.args[["control"]])] <- optim.args[["control"]]
    optim.args$control <- optimControl
    # default arguments
    optimArgs <- alist(par = optim.args$par, fn = negll, gr = negsc,
                       method = if (partial) "Nelder-Mead" else "BFGS",
                       lower = -Inf, upper = Inf,
                       control = list(), hessian = partial | !useScore)
    # user arguments
    namesOptimArgs <- names(optimArgs)
    namesOptimUser <- names(optim.args)
    optimValid <- namesOptimUser %in% namesOptimArgs
    optimArgs[namesOptimUser[optimValid]] <- optim.args[optimValid]
    if (any(!optimValid)) {
        warning("unknown names in optim.args: ",
                paste(namesOptimUser[!optimValid], collapse = ", "))
    }
    doHessian <- eval(optimArgs$hessian)
    optimMethod <- eval(optimArgs$method)


    ### Call 'optim' or 'nlminb' (default) with the above arguments

    cat("\nminimizing the negative", if (partial) "partial", "log-likelihood",
        "using", if (optimMethod!="nlminb") "'optim's", optimMethod, "...\n")
    cat("initial parameters:\n")
    print(optimArgs$par)
    if (hassiafpars && !is.null(siaf$Fcircle)) {
        .maxnpixels <- local({
            initsiaf <- optimArgs$par[grep("^e\\.siaf", names(optimArgs$par))]
            initeffRangeTypes <- rep(siaf$effRange(initsiaf),length.out=nTypes)
            initeffRanges <- initeffRangeTypes[eventTypes]
            border <- which(eps.s > bdist & initeffRanges > bdist)
            if (length(border) > 0L) {
                maxarea <- bounding.box(influenceRegion[border][[which.max(iRareas[border])]])
                inithmin <- min(initeffRangeTypes) / nCub
                ceiling(c(diff(maxarea$xrange), diff(maxarea$yrange)) / inithmin)
            } else NULL
        })
        if (!is.null(.maxnpixels) && prod(.maxnpixels) > 10000) {
            cat("\nNOTE: the initial values of the 'e.siaf' parameters potentially require up to",
                "\n     ", .maxnpixels[1], "x", .maxnpixels[2], "pixels for the midpoint cubature.",
                "If iterations are slow,",
                "\n      consider other values increasing 'eps=siaf$effRange(siafpars)/nCub'.\n\n")
        }
    }
    optimRes1 <- if (optimMethod == "nlminb") {
            nlminbControl <- optimArgs$control[c("maxit","REPORT","abstol","reltol")]
            names(nlminbControl) <- c("iter.max", "trace", "abs.tol", "rel.tol")
            if (optimArgs$control$trace == 0L) nlminbControl$trace <- 0L else {
                cat("negative log-likelihood and parameters ")
                if (nlminbControl$trace == 1L) cat("in each iteration") else {
                    cat("every", nlminbControl$trace, "iterations") }
                cat(":\n")
            }
            if (is.null(nlminbControl$rel.tol)) nlminbControl$rel.tol <- 1e-6
            # sqrt(.Machine$double.eps) is the default reltol used in optim,
            # which usually equals about 1.49e-08.
            # the default rel.tol from nlminb (1e-10) is too small,
            # nlminb did not finish despite no relevant change in loglik.
            # I therefore use 1e-6, which is also the default in package nlme (see lmeControl).
            nlminbControl <- nlminbControl[!sapply(nlminbControl, is.null)]
            nlminbRes <- nlminb(start = optimArgs$par, objective = negll,
                   gradient = negsc, hessian = fisherinfo,  # hessian only used if gradient not NULL
                   control = nlminbControl,
                   lower = optimArgs$lower, upper = optimArgs$upper)
            nlminbRes$value <- -nlminbRes$objective
            nlminbRes$counts <- nlminbRes$evaluations
            nlminbRes
        } else {
            if (finetune) optimArgs$hessian <- FALSE
            res <- do.call("optim", optimArgs)
            res$value <- -res$value
            res
        }


    ### Optional fine-tuning of ML estimates by robust Nelder-Mead

    optimRes <- if (finetune) {
            cat("\nMLE from first optimization:\n")
            print(optimRes1$par)
            cat("loglik(MLE) =", optimRes1$value, "\n")
            cat("\nfine-tuning MLE using Nelder-Mead optimization...\n")
            optimArgs$par <- optimRes1$par
            optimArgs$method <- "Nelder-Mead"
            optimArgs$hessian <- doHessian
            nmRes <- do.call("optim", optimArgs)
            nmRes$value <- -nmRes$value
            nmRes$counts[2] <- 0L   # 0 gradient evaluations (replace NA for addition below)
            nmRes
        } else optimRes1

    if (optimRes$convergence != 0) {
        cat("\nWARNING: OPTIMIZATION ROUTINE DID NOT CONVERGE",
            if (optimMethod != "nlminb") paste("(code ", optimRes$convergence, ")", sep=""),
            "!\n")
        if (!is.null(optimRes$message) && nchar(optimRes$message) > 0L) {
            cat("MESSAGE: \"", optimRes$message, "\"\n", sep="")
        }
    }

    cat("\n", if (finetune) "final ", "MLE:\n", sep = "")
    print(optimRes$par)
    cat("loglik(MLE) =", optimRes$value, "\n")






    ##############
    ### Return ###
    ##############


    ### Set up list object to be returned

    fit <- list( coefficients = optimRes$par,
        loglik = structure(optimRes$value, partial = partial),
        counts = optimRes1$counts + if (finetune) optimRes$counts else 0L,
        converged = (optimRes$convergence == 0) )


    ### Add Fisher information matrices

    # estimation of the expected Fisher information matrix
    if (useScore) fit$fisherinfo <- fisherinfo(fit$coefficients)

    # If requested, add observed fisher info (= negative hessian at maximum)
    if (!is.null(optimRes$hessian)) {
        fit$fisherinfo.observed <- -optimRes$hessian
    }


    ### Add fitted intensity values and integrated intensities at events

    # final coefficients
    theta   <- fit$coefficients
    beta0   <- theta[seq_len(nbeta0)]
    beta    <- theta[nbeta0+seq_len(p)]
    gamma    <- theta[nbeta0+p+seq_len(q)]
    siafpars <- theta[nbeta0+p+q+seq_len(nsiafpars)]
    tiafpars <- theta[nbeta0+p+q+nsiafpars+seq_len(ntiafpars)]

    # fitted intensities
    hEvents <- if (hash) .hEvents(unname(beta0), beta) else rep.int(0, Nin)
    eEvents <- if (hase) {
            gammapred <- drop(exp(mme %*% gamma)) # N-vector
            .eEvents(gammapred, siafpars, tiafpars) # Nin-vector! (only 'includes' here)
        } else rep.int(0, Nin)
    fit$fitted <- hEvents + eEvents   # = lambdaEvents  # Nin-vector
    fit$fittedComponents <- cbind(h = hEvents, e = eEvents)
    rm(hEvents, eEvents)

    # cumulative intensities at event times
    fit$tau <- if (cumCIF) {
        cat("\nCalculating the fitted cumulative intensities at events...\n")
        if (hase) {
            # tiny hack such that siaf integrals are not evaluated N-fold
            siafIntsFinal <- .siafInt(siafpars)
            .siafInt.orig <- .siafInt
            .siafInt <- function (siafpars) siafIntsFinal
        }
        heIntEvents <- matrix(NA_real_, N, 2L)
        pb <- txtProgressBar(min=0, max=N, initial=0, style=3)
        for (i in 1:N) {
            heIntEvents[i,] <- heIntTWK(beta0, beta, gammapred, siafpars, tiafpars, uppert = eventTimes[i])
            setTxtProgressBar(pb, i)
        }
        close(pb)
        if (hase) .siafInt <- .siafInt.orig
        LambdaEvents <- rowSums(heIntEvents)
        names(LambdaEvents) <- rownames(mfe)
        LambdaEvents
    } else NULL
    suppressWarnings(rm(.siafInt.orig, i, pb, heIntEvents, LambdaEvents))


    ### Append model information

    fit$npars <- c(nbeta0 = nbeta0, p = p,
                   q = q, nsiafpars = nsiafpars, ntiafpars = ntiafpars)
    fit$qmatrix <- qmatrix   # -> information about nTypes and typeNames
    fit$timeRange <- c(t0, T)    # -> for simulate.twinstim's defaults
    fit$medianeps <- c(spatial = median(mfe[["(eps.s)"]]), temporal = median(mfe[["(eps.t)"]]))
    if (!model) {
        # Link formulae to the global environment such that the evaluation environment will be dropped at the end
        environment(epidemic) <- environment(endemic) <- globalenv()
    }
    fit$formula <- list(endemic = formula(endemic), epidemic = formula(epidemic),
                        siaf = siaf, tiaf = tiaf)
    if (model) {
        fit$functions <- functions
    }

    ### Return object of class "twinstim"

    cat("\nDone.\n")
    fit$call <- cl
    fit$runtime <- proc.time()[[3]] - ptm
    class(fit) <- "twinstim"
    return(fit)

}

################################################################################
### Spatial/Temporal interaction functions for the epidemic component
###
### Author: Sebastian Meyer
### $Date: 2010-11-08 21:00:47 +0100 (Mon, 08 Nov 2010) $
################################################################################


### Generates list for Gaussian spatial interaction function
# nTypes: determines the number of parameters=(log-)standard deviations of the gaussian kernel. In a multitype epidemic, the different types may share the same spatial interaction function (type-invariant), in which case nTypes=1. Otherwise nTypes should equal the number of event types of the epidemic, in which case every type has its own variance parameter.
# logsd: logical indicating if the gaussian kernel should be reparametrized such that the log-standard deviation is the parameter in question.  This avoids constrained optimisation (L-BFGS-B) or the use of 'validpars'.
# density: logical. If TRUE, the gaussian spatial interaction function will not be scaled to have maximum value of 1 at the mean c(0,0).
# effRangeMult: determines the effective range for numerical integration in terms of multiples of the parameter, i.e. with effRangeMult=6 numerical integration only considers the 6-sigma area around the event instead of the whole observation region W.
# validpars: see 'twinstim'. If logsd = FALSE, you should either use constrained optimisation (L-BFGS-B) or set 'validpars' to function (pars) pars > 0.

siaf.gaussian <- function (nTypes, logsd = TRUE, density = FALSE, effRangeMult = 6, validpars = NULL)
{
    nTypes <- as.integer(nTypes)
    stopifnot(length(nTypes) == 1L, nTypes > 0L, isScalar(effRangeMult))

    f <- function (s, pars, types) {}       # coordinate matrix s, length(types) = 1 or nrow(s)
    deriv <- function (s, pars, types) {}   # coordinate matrix s, length(types) = 1 or nrow(s)
    Fcircle <- function (r, pars, type) {}  # single radius and type
    effRange <- function (pars) {}
    simulate <- function (n, pars, type) {} # n=size of the sample, type=single type

    # helper expressions
    tmp1 <- if (logsd) expression(sds <- exp(pars)) else expression(sds <- pars)
    tmp2 <- expression(
        sLengthSquared <- rowSums(s^2),
        L <- length(sLengthSquared),
        types <- rep(types, length.out = L),
        # standard deviations vector sds may have length 1 (type-invariant siaf)
        # thus we extend its length to fit the maximum type index
        sds <- rep(sds, length.out = max(types)),
        sdss <- sds[types]
    )

    # spatial interaction function
    body(f) <- as.call(c(as.name("{"),
        tmp1, tmp2,
        expression(fvals <- exp(-sLengthSquared/2/sdss^2)),
        if (density) expression(fvals <- fvals / (2*pi*sdss^2))
    ))

    # derivative of f wrt pars
    body(deriv) <- as.call(c(as.name("{"),
        tmp1, tmp2,
        expression(deriv <- matrix(0, L, length(pars))),
        expression(frac <- sLengthSquared/2/sdss^2),
        if (nTypes == 1L) expression(colidx <- 1L) else expression(colidx <- types),
        if (logsd) { # derive f wrt psi=log(sd) !!
            if (density) {
                expression(deriv[cbind(1:L,colidx)] <- exp(-frac) / pi/sdss^2 * (frac-1))
            } else {
                expression(deriv[cbind(1:L,colidx)] <- exp(-frac) * 2*frac)
            }
        } else { # derive f wrt sd !!
            if (density) {
                expression(deriv[cbind(1:L,colidx)] <- exp(-frac) / pi/sdss^3 * (frac-1))
            } else {
                expression(deriv[cbind(1:L,colidx)] <- exp(-frac) * 2*frac/sdss)
            }
        },
        expression(deriv)
    ))

    # calculate the integral over a circular domain around 0
    body(Fcircle) <- as.call(c(as.name("{"),
        tmp1,
        expression(
            sd <- rep(sds, length.out=type)[type],
            val <- pchisq((r/sd)^2, 2)   # cf. Abramowitz&Stegun formula 26.3.24
        ),
        if (!density) expression(val <- val * 2*pi*sd^2)
    ))

    # effective integration range as a function of sd
    body(effRange) <- as.call(c(as.name("{"),
        tmp1,
        substitute(effRangeMult*sds)
    ))

    # sampler
    body(simulate) <- as.call(c(as.name("{"),
        tmp1,
        expression(
            sd <- rep(sds, length.out=type)[type],
            matrix(rnorm(2*n, mean=0, sd=sd), nrow = n, ncol = 2L)
        )
    ))

    list(f=f, deriv=deriv, Fcircle=Fcircle, effRange=effRange,
         simulate=simulate, npars=nTypes, validpars=validpars)
}


### Returns specification of constant spatial interaction

siaf.constant <- function () {
    res <- list(
        f = as.function(alist(s=, pars=, types=, rep.int(1, nrow(s))), envir = parent.frame()),
        deriv = NULL,
        Fcircle = as.function(alist(r=, pars=, types=, pi*r^2), envir = parent.frame()),
        simulate = NULL, # will be handled specially in simEpidataCS
        npars = 0L, validpars = NULL
    )
    attr(res, "constant") <- TRUE
    res
}


### Generates list for Exponential temporal interaction function
# nTypes: cf. parameter description for siaf.gaussian

tiaf.exponential <- function (nTypes)
{
    # time points vector t, length(types) = 1 or length(t)
    g <- function (t, alpha, types) {
        types <- rep(types, length.out = length(t))
        alphat <- alpha[types]
        exp(-alphat*t)
    }
    G <- function (t, alpha, types) {
        types <- rep(types, length.out = length(t))
        alphat <- alpha[types]
        ifelse(alphat==0, t, -exp(-alphat*t)/alphat)
    }
    deriv <- function (t, alpha, types) {
        L <- length(t)
        types <- rep(types, length.out = L)
        alphat <- alpha[types]
        deriv <- matrix(0, L, length(alpha))
        deriv[cbind(1:L,types)] <- -t*exp(-alphat*t)
        deriv
    }
    Deriv <- function (t, alpha, types) {
        L <- length(t)
        types <- rep(types, length.out = L)
        alphat <- alpha[types]
        Deriv <- matrix(0, L, length(alpha))
        Deriv[cbind(1:L,types)] <- ifelse(alphat==0, -t^2/2, (t+1/alphat)*exp(-alphat*t)/alphat)
        Deriv
    }

    list(g=g, G=G, deriv=deriv, Deriv=Deriv, npars=nTypes, validpars=NULL)
}


### Returns specification of constant temporal interaction

tiaf.constant <- function () {
    res <- list(
        g = as.function(alist(t=, pars=, types=, rep.int(1, length(t))), envir = parent.frame()),
        G = as.function(alist(t=, pars=, types=, t), envir = parent.frame()),
        deriv = NULL, Deriv = NULL, npars = 0L, validpars = NULL
    )
    attr(res, "constant") <- TRUE
    res
}



### Helper function which checks if the (spatial or temporal)
### interaction function 'iaf' returns 1 at the origin 0 or (0,0)

.checkiaf0 <- function(iaf, npars, validpars, type = c("siaf", "tiaf"))
{
    type <- match.arg(type)
    if (npars > 0L) {
        testpars <- rep.int(pi, npars)
        valid <- validpars(testpars)
        if (length(valid) != 1 || !is.logical(valid)) {
            stop("the '", type, "'/'validpars' function must return a single ",
                 "logical value")
        }
        if (!valid) {
            testpars <- rep.int(0.5, npars)
            valid <- validpars(testpars)
        }
    } else valid <- TRUE
    if (valid) {
        ctext <- switch(type, siaf = "coordinate (0,0)", tiaf = "time point 0")
        iaf0 <- switch(type,
            siaf = if (npars>0L) iaf(t(c(0,0)), testpars, 1) else iaf(t(c(0,0)),,1),
            tiaf = if (npars>0L) iaf(0, testpars, 1) else iaf(0,,1))
        if (!isTRUE(all.equal(iaf0, 1))) {
            warning("the '", type, "' function should usually evaluate to 1 ",
                    "at ", ctext)
        }
    } else {
        message("make sure that the maximum value returned by the '", type,
                "' function is 1")
    }
    NULL
}



### Checks if FUN has three arguments (s/t, pars, type) and
### eventually adds the last two

.checknargs3 <- function (FUN, name)
{
    FUN <- match.fun(FUN)
    NARGS <- length(formals(FUN))
    if (NARGS == 0L) {
        stop("the function '", name, "' must accept at least one argument")
    } else if (NARGS == 1L) {
        formals(FUN) <- c(formals(FUN), alist(pars=, types=))
    } else if (NARGS == 2L) {
        formals(FUN) <- c(formals(FUN), alist(types=))
    }
    FUN
}



### Checks the siaf specification
# f: spatial interaction function (siaf). must accept two arguments, a coordinate matrix and a parameter vector. for marked twinstim, it must accept a third argument which is the type of the event (either a single type for all locations or separate types for each location)
# deriv: optional derivative of f with respect to the parameters. Takes the same arguments as f but returns a matrix with as many rows as there were coordinates in the input and npars columns. The derivative is necessary for the score function.
# Fcircle: optional function for fast calculation of the integral of f over a circle with radius r (first argument). Further arguments like f. It must not be vectorized for model fitting (will be passed single radius and single type).
# effRange: optional function returning the effective range (domain) of f for the given set of parameters such that the circle with radius effRange contains the numerical essential proportion the integral mass, e.g. function (sigma) 6*sigma. The return value must be a vector of length nTypes (effRange for each type). Must be supplied together with Fcircle.
# simulate: optional function returning a sample drawn from the spatial kernel (i.e. a two-column matrix of 'n' points). The arguments are 'n' (size of the sample), 'pars' (parameter vector of the spatial kernel), and 'type' (a single type of event being generated).
# npars: number of parameters
# validpars: a function indicating if a specific parameter vector is valid. Not necessary if npars == 0. If missing or NULL, it will be set to function (pars) TRUE. This function is rarely needed in practice, because usual box constrained parameters can be taken into account by using L-BFGS-B as the optimization method (with arguments 'lower' and 'upper').
# knots: not implemented. Knots (> 0) of a spatial interaction STEP function of the distance

checksiaf <- function (f, deriv, Fcircle, effRange, simulate, npars, validpars, knots)
{
    # if siaf is a step function specified by knots
    if (!missing(knots)) {
        return(sort(unique(as.vector(knots,mode="numeric")), decreasing=FALSE))
    }

    # if siaf is a continuous function
    npars <- as.integer(npars)
    if (length(npars) != 1 || npars < 0L) {
        stop("'siaf$npars' must be a single nonnegative number")
    }
    f <- .checknargs3(f, "siaf$f")
    haspars <- npars > 0L
    if (!haspars || missing(deriv)) deriv <- NULL
    if (!is.null(deriv)) deriv <- .checknargs3(deriv, "siaf$deriv")
    if (missing(Fcircle)) Fcircle <- NULL
    if (is.null(Fcircle) || missing(effRange)) effRange <- NULL
    if (!is.null(Fcircle)) {
        Fcircle <- .checknargs3(Fcircle, "siaf$Fcircle")
        effRange <- match.fun(effRange)
      }
    if (!is.null(effRange) && length(formals(effRange)) < 1L) {
        stop("the 'siaf$effRange' function must accept a parameter vector")
    }
    #Check if simulation function has proper format
    if (missing(simulate)) simulate <- NULL
    if (!is.null(simulate)) {
      simulate <- match.fun(simulate)
      if (length(formals(simulate)) < 3L) {
        stop("the 'siaf$simulate' function must accept three arguments")
      }
    }
    #Check if the validpars are of correct form
    validpars <- if (!haspars) NULL else if (missing(validpars) || is.null(validpars)) {
      as.function(alist(pars=, TRUE), envir = parent.frame(3))
    } else match.fun(validpars)
    .checkiaf0(f, npars, validpars, "siaf")
    #Done, return result.
    list(f = f, deriv = deriv, Fcircle = Fcircle, effRange = effRange,
         simulate = simulate, npars = npars, validpars = validpars)
}



### Checks the tiaf specification
# g: temporal interaction function (tiaf). must accept two arguments, a vector of time points and a parameter vector. for marked twinstim, it must accept a third argument which is the type of the event (either a single type for all locations or separate types for each location)
# G: a primitive of g. Must accept same arguments as g. (_vector_ of time points, not just a single one!)
# deriv: optional derivative of g with respect to the parameters. Takes the same arguments as g but returns a matrix with as many rows as there were time points in the input and npars columns. The derivative is necessary for the score function.
# Deriv: optional primitive of deriv (with respect to time). Must accept same arguments as deriv and g. Returns a matrix with as many rows as there were time points in the input and npars columns. The integrated derivative is necessary for the score function.
# npars: number of parameters, which are passed as second argument to g and G
# validpars: a function indicating if a specific parameter vector is valid. Not necessary if npars == 0. If missing or NULL, will be set to function (pars) TRUE. This function is rarely needed in practice, because usual box constrained parameters can be taken into account by using L-BFGS-B as the optimization method (with arguments 'lower' and 'upper').
# knots: not implemented. Knots (> 0) of a temporal interaction STEP function

checktiaf <- function (g, G, deriv, Deriv, npars, validpars, knots)
{
    # if tiaf is a step function specified by knots
    if (!missing(knots)) {
        stop("'knots' are not implemented for 'tiaf'")
        return(sort(unique(as.vector(knots,mode="numeric")), decreasing=FALSE))
    }

    # if tiaf is a continous function
    npars <- as.integer(npars)
    if (length(npars) != 1 || npars < 0L) {
        stop("'tiaf'/'npars' must be a single nonnegative number")
    }
    haspars <- npars > 0L
    g <- .checknargs3(g, "tiaf$g")
    G <- .checknargs3(G, "tiaf$G")
    if (!haspars || missing(deriv)) deriv <- NULL
    if (!haspars || missing(Deriv)) Deriv <- NULL
    if (!is.null(deriv)) deriv <- .checknargs3(deriv, "tiaf$deriv")
    if (!is.null(Deriv)) Deriv <- .checknargs3(Deriv, "tiaf$Deriv")
    validpars <- if (!haspars) NULL else if (missing(validpars) || is.null(validpars)) {
            as.function(alist(pars=, TRUE), envir = parent.frame(3))
        } else match.fun(validpars)
    .checkiaf0(g, npars, validpars, "tiaf")
    list(g = g, G = G, deriv = deriv, Deriv = Deriv, npars = npars, validpars = validpars)
}



### Wrapper used in main function to evaluate the siaf and tiaf arguments.
### If succesful, returns checked interaction function

.parseiaf <- function (iaf)
{
    name <- as.character(substitute(iaf))
    if (missing(iaf) || is.null(iaf)) {
        message("no ", switch(name, siaf="spatial", tiaf="temporal"),
                " interaction function in model")
        NULL
    } else if (is.list(iaf)) {
        ret <- do.call(paste("check",name,sep=""), args = iaf)
        attr(ret, "constant") <- attr(iaf, "constant")
        ret
    } else if (is.vector(iaf, mode = "numeric")) {
        stop("'knots' are not implemented for '",name,"'")
        do.call(paste("check",name,sep=""), args = list(knots = iaf))
    } else {
        stop("'",name,"' must be NULL (or missing), a list (-> continuous ",
            "function), or numeric (-> knots of step function)")
    }
}

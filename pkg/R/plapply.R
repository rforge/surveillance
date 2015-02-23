################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Parallelized lapply (wrapping around mclapply and parLapply)
### taking care of the random seed and printing progress information
###
### Copyright (C) 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


plapply <- function (X, FUN, ...,
                     .cores = 1, .cl = NA, .seed = NULL, .verbose = TRUE)
{
    if (!(useCluster <- !identical(NA, .cl))) {
        stopifnot(isScalar(.cores), .cores >= 1)
        .cores <- as.integer(.cores)
    }
    FUN <- match.fun(FUN)
    .FUN <- if (useCluster || is.primitive(FUN)) {
        FUN  # no support for reporting to the master || add.on.exit
    } else { # be verbose on.exit of FUN
        verboseExpr <- if (isTRUE(.verbose)) {
            ## progress bar or dots
            if (.cores == 1L && interactive()) {
                env <- new.env(hash = FALSE, parent = environment(FUN))
                environment(FUN) <- env  # where the progress bar lives
                env$pb <- txtProgressBar(min = 0, max = length(X), initial = 0, style = 3)
                on.exit(close(env$pb))
                quote(setTxtProgressBar(pb, pb$getVal() + 1L))
            } else {
                on.exit(cat("\n"))
                quote(cat("."))
            }
        } else if (is.call(.verbose) || is.expression(.verbose)) {
            ## custom call or expression
            .verbose
        } else if (is.character(.verbose)) {
            ## custom progress symbol
            on.exit(cat("\n"))
            substitute(cat(.verbose))
        } # else NULL (no output)
        ## add on.exit(verboseExpr) to body(FUN)
        do.call(add.on.exit, list(FUN, verboseExpr))
    }
    
    ## set random seed for reproducibility
    if (!is.null(.seed)) {
        if (useCluster) {
            parallel::clusterSetRNGStream(cl = .cl, iseed = .seed)
        } else {
            if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
                set.seed(NULL)  # initialize
            }
            .orig.seed <- get(".Random.seed", envir = .GlobalEnv)
            on.exit(assign(".Random.seed", .orig.seed, envir = .GlobalEnv),
                    add = TRUE)
            set.seed(seed = .seed, kind = if (.cores > 1L) "L'Ecuyer-CMRG")
        }
    }

    ## rock'n'roll
    if (useCluster) {
        parallel::parLapply(cl = .cl, X = X, fun = .FUN, ...)
    } else if (.cores == 1L) {
        lapply(X = X, FUN = .FUN, ...)
    } else { # use forking
        parallel::mclapply(X = X, FUN = .FUN, ...,
                           mc.preschedule = TRUE, mc.set.seed = TRUE,
                           mc.silent = FALSE, mc.cores = .cores)
    }
}


## add an on.exit() statement at the beginning of a function
add.on.exit <- function (FUN, expr)
{
    FUN <- match.fun(FUN)
    if (is.null(expr <- substitute(expr))) {
        return(FUN)
    }
    if (is.primitive(FUN)) { # body(FUN) is NULL
        stop("not implemented for primitive functions")
    }
    onexitexpr <- substitute(on.exit(expr))
    obody <- body(FUN)
    body(FUN) <- if (is.call(obody) && identical(as.name("{"), obody[[1L]])) {
        ## body(FUN) is a braced expression (usual case)
        ## and we insert on.exit(expr) directly after "{"
        as.call(append(x = as.list(obody), values = onexitexpr, after = 1L))
    } else {
        ## body(FUN) is a symbol or a single call like UseMethod("print")
        as.call(c(as.name("{"), onexitexpr, obody))
    }
    FUN
}

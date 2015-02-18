################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Monte Carlo Permutation Test for Space-Time Interaction in "twinstim"
###
### Copyright (C) 2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

epitest <- function (model, data, B = 199, seed = NULL,
                     cores = 1, verbose = TRUE)
{
    stopifnot(inherits(model, "twinstim"), inherits(data, "epidataCS"),
              model$converged, isScalar(B), B >= 1, isScalar(cores), cores >= 1,
              length(verbose) == 1L)
    B <- as.integer(B)
    cores <- as.integer(cores)
    if (model$npars["q"] == 0L) {
        stop("no epidemic component in 'model'")
    }
    
    ## auxiliary function to compute the LRT statistic
    lrt <- function (m0, m1) {
        l0 <- m0$loglik
        l1 <- m1$loglik
        c(l0 = l0, l1 = l1, D = 2 * (l1 - l0),
          converged = isTRUE(m1$converged) && isTRUE(m0$converged))
    }
    
    ## observed test statistic
    t0 <- model$timeRange[1L]
    m0 <- update.twinstim(model, epidemic = ~0, siaf = NULL, tiaf = NULL,
                          control.siaf = NULL, model = FALSE, cumCIF = FALSE,
                          cores = 1, verbose = FALSE,
                          optim.args = list(control = list(trace = 0)))
    if (!isTRUE(m0$converged)) {
        stop("endemic-only model did not converge")
    }
    LRT <- lrt(m0 = m0, m1 = model)
    STATISTIC <- structure(LRT["D"], l0 = LRT[["l0"]], l1 = LRT[["l1"]])
    
    ## progress bar
    increment_pb <- if (is.character(verbose)) { # progress symbol
        on.exit(cat("\n"))
        function (...) cat(verbose)
    } else {
        verbose <- as.integer(verbose)
        if (verbose <= 0L) {        # no output
            function (...) {}
        } else if (verbose == 1L) { # progress bar or dots
            if (cores == 1L) {
                pb <- txtProgressBar(min = 0, max = B, initial = 0, style = 3)
                on.exit(close(pb))
                function (...) setTxtProgressBar(pb, pb$getVal() + 1L)
            } else {
                on.exit(cat("\n"))
                function (...) cat(".")
            }
        } else if (verbose >= 2L) { # print test statistics
            cat("Endemic/Epidemic log-likelihoods and LRT statistic:\n",
                paste0(names(LRT)[1:3], " = ", sprintf("%4.1f", LRT[1:3]),
                       collapse = " | "),
                "\n\nResults from B=", B, " permutations of the event times:\n",
                sep = "")
            function (m0, m1) {
                lrt <- lrt(m0, m1)
                cat(paste0(names(lrt)[1:3], " = ", sprintf("%4.1f", lrt[1:3]),
                           collapse = " | "))
                if (!lrt["converged"]) {
                    msg <- c(m0 = m0$converged, m1 = m1$converged)
                    msg <- msg[msg != "TRUE"]
                    cat(" | WARNING (", paste0(names(msg), collapse = " and "),
                        "): ", paste0(unique(msg), collapse = " and "), sep = "")
                }
                cat("\n")
            }
        }
    }
    
    ## set random seed for reproducibility
    if (!is.null(seed)) {
        if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            set.seed(NULL)
        .orig.seed <- get(".Random.seed", envir = .GlobalEnv)
        on.exit(assign(".Random.seed", .orig.seed, envir = .GlobalEnv),
                add = TRUE)
        set.seed(seed = seed, kind = if (cores > 1L) "L'Ecuyer-CMRG")
        ##parallel::mc.reset.stream()  # redundant, mclapply() takes care itself
    }
    
    ## define the function to be replicated B times:
    ## permute data, update epidemic model, compute endemic-only model
    permfits1 <- function (...) {
        .permdata <- permute.epidataCS(data, what = "time", keep = time <= t0)
        m1 <- update.twinstim(model, data = .permdata,
                              model = FALSE, cumCIF = FALSE,
                              cores = 1, verbose = FALSE,
                              optim.args = list(control = list(trace = 0)))
        m0 <- update.twinstim(m1, epidemic = ~0, siaf = NULL, tiaf = NULL,
                              control.siaf = NULL)
        increment_pb(m0, m1)
        list(m0 = m0, m1 = m1)
    }

    ## rock'n'roll (the computationally intensive part)
    permfits <- if (cores == 1L) {
        lapply(X = integer(B), FUN = permfits1)
    } else {
        parallel::mclapply(X = integer(B), FUN = permfits1,
                           mc.preschedule = TRUE, mc.set.seed = TRUE,
                           mc.silent = FALSE, mc.cores = cores)
    }
    
    ## compute the test statistic
    permstats <- as.data.frame(t(vapply(
        X = permfits, FUN = do.call, what = lrt, 
        FUN.VALUE = numeric(4L), USE.NAMES = TRUE
    )))
    permstats$converged <- as.logical(permstats$converged)
    
    ## compute permutation-based p-value
    PVAL <- mean(c(STATISTIC, permstats[["D"]][permstats[["converged"]]]) >= STATISTIC)
    ## asymptotic p-value for comparison (invalid)
    attr(PVAL, "chisq") <- pchisq(c(STATISTIC),
                                  df = length(coef(model)) - length(coef(m0)),
                                  lower.tail = FALSE)
    
    ## gather results
    res <- list(
        method = paste("Permutation Test for Space-Time Interaction\n\t",
            "(based on", sum(permstats[["converged"]]), "replicates)"),
        data.name = paste0(deparse(substitute(data)),
            "\ntwinstim:  ", deparse(substitute(model))),
        statistic = STATISTIC,
        p.value = PVAL,
        permfits = permfits,
        permstats = permstats
    )
    class(res) <- "htest"
    res
}

### Experimental support for "hhh4" models in memisc::mtable()
### Copyright (C) 2019 Sebastian Meyer under GNU GPL-2

## CAVE: this currently ignores sd.corr parameters
## CAVE: idx2Exp cannot be used with indices since these will vary over models
getSummary.hhh4 <- function (obj, alpha = 0.05, reparamPsi = TRUE,
                             idx2Exp = TRUE, amplitudeShift = TRUE, ...)
{
    coefSE <- fixef(obj, se = TRUE, reparamPsi = reparamPsi,
                    idx2Exp = idx2Exp, amplitudeShift = amplitudeShift)
    ci <- confint(obj, parm = seq_len(obj$dim["fixed"]), level = 1-alpha,
                  reparamPsi = reparamPsi, idx2Exp = idx2Exp,
                  amplitudeShift = amplitudeShift)
    ## array of coefficients
    if (FALSE) { # experimental: "multiple equations" layout (3rd dimension)
        model <- terms(obj)
        coefSE <- as.data.frame(coefSE)
        coefSE$part <- c("ar","ne","end")[unlist(model$terms["offsetComp",])]
        coefSE$covar <- unlist(model$terms["name",])
        covars <- unique(coefSE$covar)
        parts <- unique(coefSE$part)
        ca <- array(
            dim = c(length(covars), 6, length(parts)),
            dimnames = list(covars, c("est", "se", "stat", "p", "lwr", "upr"), parts)
        )
        for (.part in parts) {
            .partcoefSE <- subset(coefSE, part == .part)
            ca[.partcoefSE$covar, c(1,2), .part] <- as.matrix(.partcoefSE[1:2])
        }
    } else {
        ca <- array(
            data = cbind(coefSE, NA, NA, ci),
            dim = c(nrow(coefSE), 6, 1),
            dimnames = list(
                sub("ri\\((iid|car)\\)", "1", rownames(coefSE)), # intercept
                c("est", "se", "stat", "p", "lwr", "upr"),
                paste0(surveillance:::componentsHHH4(obj), collapse = "+")
            )
        )
    }
    maxEV_range <- unique(range(getMaxEV(obj)))
    if (length(maxEV_range) > 1)
        maxEV_range <- paste(sprintf("%.2f", maxEV_range), collapse = " -- ")
    list(coef = ca,
         sumstat = c(
             N = nobs(obj), logLik = as.vector(logLik(obj)), AIC = AIC(obj),
             margll = obj$margll, maxEV = maxEV_range
         ),
         ## contrasts = NULL, xlevels = NULL,
         call = obj$call)
}

setSummaryTemplate("hhh4" = c(
  "N" = "($N:d)",
  "Log-likelihood" = "($logLik:f#)",
  "Marginal log-likelihood" = "($margll:f#)", "AIC" = "($AIC:f#)",
  "Max-EV" = "($maxEV:#)"
  ))

## options("summary.stats.hhh4" = names(getSummaryTemplate("hhh4")), # this shouldn't be necessary
##         "signif.symbols" = NULL)


if (FALSE) {  # example
    ## fit some models
    library("surveillance")
    data("measlesWeserEms")
    m0 <- hhh4(measlesWeserEms)
    m1 <- update(m0, family = "NegBin1")
    m2 <- update(m1, S = list(end = 1))
    m3 <- update(m2, ar = list(f = ~1))
    m4 <- update(m3, ne = list(f = ~1, weights = W_powerlaw(maxlag = 5, log = TRUE)))
    ## table of model summaries
    mtable(m0, m1, m2, m3, m4,
           summary.stats = c("N", "Log-likelihood", "AIC", "Max-EV"),
           signif.symbols = NULL, digits = 2)
}

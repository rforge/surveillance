suppressPackageStartupMessages(library("surveillance", quietly = TRUE))

## fit a complex hhh4 model (without random effects)
data("measlesWeserEms")
measlesModel <- list(
    end = list(f = addSeason2formula(~1 + t, S=1, period=52),
               offset = population(measlesWeserEms)),
    ar = list(f = ~1),
    ne = list(f = ~1 + log(pop), weights = W_powerlaw(maxlag = 5)),
    family = "NegBin1", data = list(pop = population(measlesWeserEms))
)
measlesFit <- hhh4(measlesWeserEms, measlesModel)
summary(measlesFit, maxEV = TRUE)

## check score function and fisher info against numerical approximations
if (requireNamespace("numDeriv", quietly = TRUE)) {
    capture.output(
        pencomp <- hhh4(measlesWeserEms, measlesModel,
                        check.analyticals = "numDeriv")$pen
    )
    stopifnot(
        all.equal(pencomp$score$analytic, pencomp$score$numeric,
                  tolerance = .Machine$double.eps^0.5),
        all.equal(pencomp$fisher$analytic, pencomp$fisher$numeric,
                  tolerance = .Machine$double.eps^0.25)
    )
}

context("Fixed effects hhh4() model fit and involved analytical derivatives")

data("measlesWeserEms")
measlesModel <- list(
    end = list(f = addSeason2formula(~1 + t, S=1, period=52),
               offset = population(measlesWeserEms)),
    ar = list(f = ~1),
    ne = list(f = ~1 + log(pop), weights = W_powerlaw(maxlag = 5)),
    family = "NegBin1", data = list(pop = population(measlesWeserEms))
)

measlesFit <- hhh4(stsObj = measlesWeserEms, control = measlesModel)

test_that("estimates and standard errors are reproducible", {
    ## dput(coef(measlesFit, se = TRUE))
    orig <- structure(
        c(-0.499636482022272, 0.551345030080107, 0.96093157194767, 
          -0.153585641356373, 0.00333284018297979, 1.01500011496702,
          -0.588738943313705, 5.52782609236691, 1.81915612994789,
          0.121781347106564, 1.27401298230559, 0.453889365025671,
          0.281013375484401, 0.00459840327748742, 0.210642721317572, 
          0.191921649336323, 1.87984346848385, 0.265016986696184),
        .Dim = c(9L, 2L),
        .Dimnames = list(c("ar.1", "ne.1", "ne.log(pop)", "end.1", 
            "end.t", "end.sin(2 * pi * t/52)", "end.cos(2 * pi * t/52)", 
            "neweights.d", "overdisp"), c("Estimate", "Std. Error"))
    )
    expect_that(coef(measlesFit, se = TRUE), equals(orig))
})

test_that("score vector and Fisher info agree with numerical approximations", {
    if (!requireNamespace("numDeriv", quietly = TRUE)) {
        skip("package \"numDeriv\" is not installed")
    }
    pencomp <- hhh4(measlesWeserEms, measlesModel,
                    check.analyticals = "numDeriv")$pen
    expect_that(pencomp$score$analytic,
                equals(pencomp$score$numeric,
                       tolerance = .Machine$double.eps^0.5))
    expect_that(pencomp$fisher$analytic,
                equals(pencomp$fisher$numeric,
                       tolerance = .Machine$double.eps^0.25))
})

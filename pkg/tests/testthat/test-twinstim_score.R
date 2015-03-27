context("Likelihood and score function of twinstim()")
## Note: derivatives of interaction functions are tested in separate files
##       we thus use relatively fast step functions here

data("imdepi")
model <- twinstim(
    endemic = addSeason2formula(~offset(log(popdensity)),
        S = 1, period = 365, timevar = "start"),
    epidemic = ~type,
    siaf = siaf.step(c(5, 20), maxRange = 50),
    tiaf = tiaf.step(2),
    data = imdepi,
    optim.args = NULL, verbose = FALSE
)
theta <- c("h.(Intercept)" = -20,
           "h.sin(2 * pi * start/365)" = 0.2, "h.cos(2 * pi * start/365)" = 0.3,
           "e.(Intercept)" = -10, "e.typeC" = -0.9,
           "e.siaf.1" = -1, "e.siaf.2" = -3, "e.tiaf.1" = -1)

test_that("likelihood is still the same", {
    expect_that(model$ll(theta), equals(-9610.68695991737))
})

test_that("score vector agrees with numerical approximation", {
    numsc <- if (surveillance.options("allExamples")) {
        numDeriv::grad(func = model$ll, x = theta)
    } else { # for faster --as-cran tests
        c(-365.19927878021, -29.3546236207476, -45.8139085706014,
          -88.5862997849202, -24.0808271983838, -54.0273836522059,
          -28.0233414216383, -74.5539641345285)
    }
    expect_that(model$sc(theta), equals(numsc))
})

## Note: twinstim() uses an estimate of the _expected_ Fisher information,
##       which does not necessarily agree with the negative Hessian of the ll
##       (it does asymptotically at the MLE)
## numfi <- -numDeriv::hessian(func = model$ll, x = theta)
## anafi <- model$fi(theta)

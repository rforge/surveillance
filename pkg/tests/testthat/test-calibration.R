context("Calibration tests for Poisson or NegBin predictions")

mu <- c(0.1, 1, 3, 6, pi, 100)
size1 <- 0.5
size2 <- c(0.1, 0.1, 10, 10, 100, 100)
##set.seed(2); y <- rnbinom(length(mu), mu = mu, size = size1)
y <- c(0, 0, 2, 14, 5, 63)

test_that("still the same DSS z-statistics", {
    zP <- calibrationTest(y, mu, which = "dss")$statistic
    expect_equal(zP, 6.07760977730636, check.attributes = FALSE)

    zNB1 <- calibrationTest(y, mu, size1, which = "dss")$statistic
    expect_equal(zNB1, -0.468561113465647, check.attributes = FALSE)

    zNB2 <- calibrationTest(y, mu, size2, which = "dss")$statistic
    expect_equal(zNB2, 2.81071829075294, check.attributes = FALSE)
})


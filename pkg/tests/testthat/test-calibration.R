context("Calibration tests for Poisson or NegBin predictions")

mu <- c(0.1, 1, 3, 6, pi, 100)
size1 <- 0.5
size2 <- c(0.1, 0.1, 10, 10, 100, 100)
##set.seed(2); y <- rnbinom(length(mu), mu = mu, size = size1)
y <- c(0, 0, 2, 14, 5, 63)

zExpected <- rbind(
    dss = c(P = 6.07760977730636, NB1 = -0.468561113465647, NB2 = 2.81071829075294),
    logs = c(P = 5.95656242588096, NB1 = 0.403872251419915, NB2 = 2.77090543018323)
    )

for (score in rownames(zExpected)) {
    .zExpected <- zExpected[score, , drop = TRUE]
    test_that(paste0("still the same z-statistics with ", score), {
        ## Poisson predictions
        zP <- calibrationTest(y, mu, which = score)$statistic
        expect_equal(zP, .zExpected["P"], check.attributes = FALSE)
        ## NegBin predictions with common size parameter
        zNB1 <- calibrationTest(y, mu, size1, which = score)$statistic
        expect_equal(zNB1, .zExpected["NB1"], check.attributes = FALSE)
        ## NegBin predictions with varying size parameter
        zNB2 <- calibrationTest(y, mu, size2, which = score)$statistic
        expect_equal(zNB2, .zExpected["NB2"], check.attributes = FALSE)
    })
}

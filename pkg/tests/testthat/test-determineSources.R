context("Determine list of potential sources in \"epidataCS\"")

data("imdepi")

test_that("determineSourcesC() yields same result as old implementation", {
    sources <- determineSources(imdepi$events$time, imdepi$events$eps.t,
                                coordinates(imdepi$events), imdepi$events$eps.s,
                                imdepi$events$type, imdepi$qmatrix)
    expect_identical(sources, imdepi$events$.sources)
})

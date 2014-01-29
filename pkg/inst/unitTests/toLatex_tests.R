# dependencies
library("testthat")
library("surveillance")

test_that("toLatex exepts basic input and returns Latex", {
  data("ha.sts")
  control <- list(
    noPeriods=10,populationBool=FALSE,
    fitFun="algo.farrington.fitGLM.flexible",
    b=4,w=3,weightsThreshold=2.58,
    pastWeeksNotIncluded=26,
    pThresholdTrend=1,trend=TRUE,
    thresholdMethod="new",alpha=0.01
  )
  result <- ha.sts
  result@alarm[,7]  <- TRUE
  result@upperbound[,7]  <- 1
  laTex <- toLatex(result, subset=(280:290), table.placement="h", size = "scriptsize",
                       sanitize.text.function = identity,
                       NA.string = "-",include.rownames=FALSE)
  
  laTex2 <- toLatex(list(result, result, result), subset=(280:290), 
                        table.placement="h", size = "scriptsize",
                       sanitize.text.function = identity,
                       NA.string = "-",include.rownames=FALSE)
  
  laTex3 <- toLatex(result, subset=(280:290),
                    alarmPrefix = "aaaa",
                    alarmSuffix = "bbbb", table.placement="h", size = "scriptsize",
                   sanitize.text.function = identity,
                   NA.string = "-",include.rownames=FALSE)

  expect_true(grepl("aaaa", paste(as.character(laTex3), collapse = ' ')))
  expect_true(grepl("bbbb", paste(as.character(laTex3), collapse = ' ')))
  expect_equal(class(laTex), "Latex")
  expect_equal(class(laTex2), "Latex")
})

test_that("toLatex test caption", {
  data("ha.sts")
  testCaption <- "Please print my caption"
  latex <- toLatex(ha.sts, caption = testCaption)
  expect_true(grepl(testCaption, paste(as.character(latex), collapse = ' ')))
})

test_that("toLatex ubColumnLabel", {
  data("ha.sts")
  testUBLabel <- "Upperbound"
  latex <- toLatex(ha.sts, ubColumnLabel = testUBLabel)
  expect_true(grepl(testUBLabel, paste(as.character(latex), collapse = ' ')))
})

test_that("toLatex no input", {
  expect_error(toLatex(null), "")
})

test_that("algo test", {
  # Create a test object
  data("salmonella.agona")
  # Create the corresponding sts object from the old disProg object
  salm <- disProg2sts(salmonella.agona)
  ### RUN THE ALGORITHMS WITH TWO DIFFERENT SETS OF OPTIONS ###
  # Farrington with old options
  control1 <- list(range=(260:312),
                   noPeriods=1,populationOffset=FALSE,
                   fitFun="algo.farrington.fitGLM.flexible",
                   b=4,w=3,weightsThreshold=1,
                   pastWeeksNotIncluded=3,
                   pThresholdTrend=0.05,trend=TRUE,
                   thresholdMethod="delta",alpha=0.1)
  salm1 <- farringtonFlexible(salm,control=control1)
  
  expect_equal(class(toLatex(salm1)), "Latex")
})

test_that("toLatex wrong subset", {
  data("ha.sts")
  expect_error(toLatex(ha.sts, subset=(-5:290)), "")
  expect_error(toLatex(ha.sts, subset=(1:10000)), "")
  expect_error(toLatex(ha.sts, subset=(10000:100000)), "")
})
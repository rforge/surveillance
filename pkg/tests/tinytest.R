### lazy (!) transition from "testthat" to "tinytest"
### Copyright (C) 2020 Sebastian Meyer

if (!requireNamespace("tinytest", quietly = TRUE)
    || packageVersion("tinytest") < "1.2.3.6") {
    message("this test suite requires package 'tinytest' (> 1.2.3)")
    q("no")
}

## provide simple replacement for test_that() expectation bundles
test_that <- function (desc, code) {
    eval(substitute(code), new.env(parent = parent.frame()))
    invisible()
}

## expect_is wrapper (as long as there is no tinytest::expect_inherits)
expect_is <- function (current, class, info = NA_character_) {
    ## find captured expectation function from tinytest::run_test_file
    expect_true <- get("expect_true", parent.frame(), inherits = TRUE)
    ## with tinytest::expect_true(), these tests would silently be ignored!
    expect_true(inherits(current, class), info = info)
}

## we use verbose = 1 to print_status() only after each test file,
## not after each expression (verbose = 2)
tinytest::test_package("surveillance", testdir = "testthat", verbose = 1)

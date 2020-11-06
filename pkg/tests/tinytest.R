### lazy (!) transition from "testthat" to "tinytest"
### Copyright (C) 2020 Sebastian Meyer

if (!requireNamespace("tinytest", quietly = TRUE)) {
    message("this test suite requires package 'tinytest'")
    q("no")
}

## provide simple replacement for test_that() expectation bundles
test_that <- function (desc, code) {
    eval(substitute(code), new.env(parent = parent.frame()))
    invisible()
}

## expect_is wrapper (as long as there is no tinytest::expect_inherits)
expect_is <- function (current, class, info = NA_character_) {
    tinytest::expect_true(inherits(current, class), info = info)
}

## variant of tinytest::test_package() for non-installed tests;
## we use verbose = 1 to print_status() only after each test file,
## not after each expression (verbose = 2)
.test_package <- function (pkgname, testdir, at_home = FALSE,
                           verbose = 1, ...)
{
    library(pkgname, character.only = TRUE)
    
    ## run test files and collect results
    out <- tinytest::run_test_dir(testdir, at_home = at_home,
                                  verbose = verbose, ...)
    
    ## throw an error if any test fails
    i_fail <- vapply(out, function (x) !is.na(x) && !x, TRUE)
    if (any(i_fail) && !interactive()) {
        ## msg <- paste0(vapply(out[i_fail], format, "", type = "long"), collapse = "\n")
        ## Such an error msg could become very long and would be truncated,
        ## so we simply print the summary and stop().
        ## R CMD check will show the last few lines of output.
        print(out)
        stop("test failure", call. = FALSE)
    }
    
    out
}

.test_package("surveillance", testdir = "testthat")

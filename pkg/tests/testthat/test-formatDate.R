context("ISO8601 Date conversion on Windows and Linux")

#Generate some dates
d <- as.Date("2001-01-01")
d2 <- as.Date(c("2001-01-01","2002-05-01"))

test_that("formatting date", expect_that(formatDate(d,"W%V-%Y"), equals("W01-2001")))

test_that("Formatting date vectors with ISO8601 and UK conventions",
          expect_that(formatDate(d2,"W%V-%G / W%W-%Y / %d-%m-%Y"),
                      equals(c("W01-2001 / W01-2001 / 01-01-2001","W18-2002 / W17-2002 / 01-05-2002"))))

test_that("Formatting date vectors with roman letters for quarters",
          expect_that(formatDate(d2,"%G\n%OQ"), equals(c("2001\nI","2002\nII"))))


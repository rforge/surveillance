
## check that the "Author" and "Authors@R" fields in our DESCRIPTION are in sync
## Since R 3.0.3 this check is also performed by R CMD check if
## _R_CHECK_CRAN_INCOMING_=TRUE, e.g., if using --as-cran

meta <- packageDescription("surveillance")

aar <- utils:::.read_authors_at_R_field(meta[["Authors@R"]])
aarFormatted <- paste0(format(aar,
                              include=c("given", "family", "role", "comment")),
                       collapse=", ")

trimSpace <- function (x) gsub("[[:space:]]+", " ", x)
stopifnot(identical(trimSpace(meta[["Author"]]),
                    trimSpace(aarFormatted)))

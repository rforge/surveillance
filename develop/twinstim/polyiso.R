## debug siaf_polyCub1_iso on Solaris for d == 2.0
#setwd("../../pkg/src/")
#setwd("surveillance/pkg/src")

xypoly <- spatstat::letterR$bdry[[1]]
intrfr_code <- 10  # 30
pars <- c(1, log(2))

try(dyn.unload("twinstim_siaf_polyCub_iso.so"), silent = TRUE)
system2("R", "CMD SHLIB --clean twinstim_siaf_polyCub_iso.c",
        env = paste0("PKG_CPPFLAGS=-I'",
            system.file("include", package="polyCub"),
        "'"))
dyn.load("twinstim_siaf_polyCub_iso.so")
requireNamespace("polyCub")

res <- .C("C_siaf_polyCub1_iso", as.double(xypoly$x), as.double(xypoly$y),
          as.integer(length(xypoly$x)), as.integer(intrfr_code),
          as.double(pars), 100L, 0.0001, 0.0001, 1L,
          value=double(1L), abserr=double(1L), neval=integer(1L))
print(unlist(tail(res, 3)))

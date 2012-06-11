################################################################################
## Authors: Sebastian Meyer, Michael Hoehle
## $Date$
## Project: Preparing package build
## Note: Watch the list in .Rbuildignore
##
## History:
##    8 Jan 2008 (MH): initial version
##   29 Mar 2012 (SM): Added making of R/sysdata.rda from sysdata/sysdata.R
##    4 Apr 2012 (SM): R execution command as a variable
##   11 Jun 2012 (SM): Restored and cleaned up Makefile
## 
################################################################################

## Define variable for R executable which enables the use of alternatives,
## e.g., 'make check R=R-devel'
R=R

## sysdata file
SYSDATA=pkg/R/sysdata.rda

## phony targets
.PHONY: clean build check install manual



build: clean ${SYSDATA}
	$R CMD build --resave-data --compact-vignettes pkg

clean:
	cd pkg/src; rm -f *.o *.so *.dll symbols.rds
	rm -f pkg/*/.Rhistory

## Save internal datasets from pkg/sysdata/ into pkg/R/sysdata.rda
${SYSDATA}: pkg/sysdata/sysdata.R
	cd pkg/sysdata; $R CMD BATCH --vanilla --no-timing sysdata.R
	mv pkg/sysdata/sysdata.rda $@

check: clean ${SYSDATA}
	$R CMD check --as-cran pkg
## further option: --use-gct (for better detection of memory bugs/segfaults)

install: ${SYSDATA}
	$R CMD INSTALL pkg

manual:	
	$R CMD Rd2pdf --batch --output=manual.pdf pkg

# macsrc:
# 	cd src ; gcc-4.0 -arch i386 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -no-cpp-precomp -I/Library/Frameworks/R.framework/Resources/include -I/Library/Frameworks/R.framework/Resources/include/i386  -msse3  -D__NO_MATH_INLINES  -fPIC  -g -O2 -Wall -pedantic -c surveillance.c -o surveillance.o


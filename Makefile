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
##   18 Jun 2012 (SM): fixed make manual bug (must first remove old manual.pdf)
##   19 Jun 2012 (SM): run check on built package instead of source directory
##   27 Jun 2012 (SM): added checkUsage recipe (R package codetools)
##    9 Aug 2012 (SM): added --timings for R CMD check
##   27 Aug 2012 (SM): install from built .tar.gz (which includes vignettes)
##   14 Sep 2012 (SM): automatic date in DESCRIPTION file
##   15 Nov 2012 (SM): removed automatic date in DESCRIPTION file since R-Forge
##                     now includes date and revision in its built packages
##   10 Dec 2012 (SM): add message about warnings in examples in "check"
##    6 Feb 2014 (SM): set _R_CHECK_FORCE_SUGGESTS_ to FALSE (for INLA package)
##   25 Aug 2014 (SM): use compact-vignettes=both, i.e., including ghostscript
##    7 Jun 2015 (SM): added rule to create the NEWS.html as on CRAN
##   12 Jun 2015 (SM): added rule to check with allExamples
################################################################################

## Define variable for R which enables the use of alternatives,
## e.g., 'make check R=R-devel'
R := R

## sysdata file
SYSDATA := pkg/R/sysdata.rda
DESCRIPTION := pkg/DESCRIPTION

## package version
VERSION := $(shell $R --vanilla --slave -e 'cat(read.dcf("${DESCRIPTION}", fields="Version"))')

## svn revision number
#REVISION := $(strip $(shell svnversion pkg))
#<-CAVE: would look like 411M because of modified working copy
#=> use svn:keywords Rev file property for revision and modify date on build
#   such that the rev property will be updated too

## Date field of DESCRIPTION file
#DATE := $(shell date +%F)
# DESCRIPTION (esp. the Rev property) would not be updated across multiple
# revisions on the same day => use date _and_ time
#DATE := $(shell date --rfc-3339=seconds)
# alternative: svn last changed date (date info would be lagged by one revision)
#LASTCHANGEDDATE := $(shell svn info pkg | grep "^Last Changed Date:" | \
#                     grep -E -o "[0-9]{4}-[0-9]{2}-[0-9]{2}")
# alternative: most recent file modification date (but this includes unversioned
# files and re-savings of files without actually changing their contents)
#$(shell find . -printf "%TY-%Tm-%Td\n" | sort -nr | head -n 1)

## phony targets
.PHONY: clean build check check-allExamples install manual #${DESCRIPTION}

clean:
	cd pkg/src; rm -f *.o *.so *.dll symbols.rds
	rm -f pkg/*/.Rhistory

build: ${SYSDATA} #${DESCRIPTION}
	$R CMD build --no-resave-data --compact-vignettes=both pkg

## update date in DESCRIPTION file
# ${DESCRIPTION}:
# 	sed -i "s/^\(Date:\)[^\r]*/\1 ${DATE}/" $@

## Save internal datasets from pkg/sysdata/ into pkg/R/sysdata.rda
${SYSDATA}: pkg/sysdata/sysdata.R
	cd pkg/sysdata; $R CMD BATCH --vanilla --no-timing sysdata.R
	mv pkg/sysdata/sysdata.rda $@

check: build
	_R_CHECK_FORCE_SUGGESTS_=FALSE $R CMD check --as-cran --timings surveillance_${VERSION}.tar.gz
## further option: --use-gct (for better detection of memory bugs/segfaults)
	@echo "timings <- read.table(file.path('surveillance.Rcheck','surveillance-Ex.timings'), header=TRUE, row.names='name'); \
	timings <- timings[order(timings$$elapsed, decreasing=TRUE),'elapsed',drop=FALSE]; \
	cat(capture.output(subset(timings, elapsed > 1)), sep='\n')" | $R --slave --vanilla
	@cd surveillance.Rcheck; nwarn=`grep -c "^Warning" surveillance-Ex.Rout`; \
	if [ $$nwarn -gt 0 ]; then echo "\n\tWARNING: $$nwarn" \
        "warning(s) thrown when running examples,\n" \
	"\t         see file surveillance.Rcheck/surveillance-Ex.Rout\n"; fi

## check with all examples
check-allExamples: export _R_SURVEILLANCE_ALL_EXAMPLES_=TRUE
check-allExamples: check

install: build
	$R CMD INSTALL surveillance_${VERSION}.tar.gz

checkUsage: install
	echo "library('surveillance'); library('codetools'); \
	checkUsagePackage('surveillance', suppressFundefMismatch=FALSE, \
	    suppressLocalUnused=TRUE, suppressNoLocalFun=TRUE, skipWith=TRUE, \
	    suppressUndefined=FALSE, suppressPartialMatchArgs=FALSE)" \
	| $R --slave --no-save --no-restore

manual:	
	$R CMD Rd2pdf --batch --force --output=manual.pdf pkg

NEWS.html: pkg/inst/NEWS.Rd
	$R --vanilla --slave -e 'tools::Rd2HTML("$<", out = "$@", stylesheet = "http://cran.r-project.org/web/CRAN_web.css")'
	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

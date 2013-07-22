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
################################################################################

## Define variable for R and sed script which enables the use of alternatives,
## e.g., 'make check R=R-devel'
R = R
sed = sed

## sysdata file
SYSDATA := pkg/R/sysdata.rda
DESCRIPTION := pkg/DESCRIPTION

## package version
VERSION := $(strip $(shell grep "^Version:" ${DESCRIPTION} | cut -f 2 -d ":"))

## svn revision number
#REVISION := $(strip $(shell svnversion pkg))
#<-CAVE: would look like 411M because of modified working copy
#=> use svn:keywords Rev file property for revision and modify date on build
#   such that the rev property will be updated too

## Date field of DESCRIPTION file
#DATE := $(shell date +%F)
# DESCRIPTION (esp. the Rev property) would not be updated across multiple
# revisions on the same day => use date _and_ time
DATE := $(shell date --rfc-3339=seconds)
# alternative: svn last changed date (date info would be lagged by one revision)
#LASTCHANGEDDATE := $(shell svn info pkg | grep "^Last Changed Date:" | \
#                     grep -E -o "[0-9]{4}-[0-9]{2}-[0-9]{2}")
# alternative: most recent file modification date (but this includes unversioned
# files and re-savings of files without actually changing their contents)
#$(shell find . -printf "%TY-%Tm-%Td\n" | sort -nr | head -n 1)

## phony targets
.PHONY: clean build check install manual #${DESCRIPTION}

clean:
	cd pkg/src; rm -f *.o *.so *.dll symbols.rds
	rm -f pkg/*/.Rhistory

build: clean ${SYSDATA} #${DESCRIPTION}
	$R CMD build --no-resave-data --compact-vignettes pkg

## update date in DESCRIPTION file
# ${DESCRIPTION}:
# 	$(sed) -i "s/^\(Date:\)[^\r]*/\1 ${DATE}/" $@

## Save internal datasets from pkg/sysdata/ into pkg/R/sysdata.rda
${SYSDATA}: pkg/sysdata/sysdata.R
	cd pkg/sysdata; $R CMD BATCH --vanilla --no-timing sysdata.R
	mv pkg/sysdata/sysdata.rda $@

check: build
	$R CMD check --as-cran --no--timing surveillance_${VERSION}.tar.gz

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

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
##   17 Mar 2016 (SM): check-allExamples with --run-dontrun and --extra-arch
##   21 Jun 2017 (SM): account for R code with roxygen documentation
##   12 Jul 2017 (SM): "quick" vs. CRAN-versions of build and check rules
##   13 Sep 2018 (SM): drop roxygen (no longer supports latin1 packages)
##   02 Nov 2018 (SM): add pkgdown recipe
################################################################################

## Define variable for R which enables the use of alternatives,
## e.g., 'make check R=R-devel'
R := R

## sysdata file
SYSDATA := pkg/R/sysdata.rda

## package version
VERSION := $(shell $R --vanilla --slave -e 'cat(read.dcf("pkg/DESCRIPTION", fields="Version"))')

## build the package
BUILD_COMPACT_VIGNETTES := no
build: ${SYSDATA}
	$R CMD build --no-resave-data --compact-vignettes=${BUILD_COMPACT_VIGNETTES} pkg
build-cran: BUILD_COMPACT_VIGNETTES := both
build-cran: build

## Save internal datasets from pkg/sysdata/ into pkg/R/sysdata.rda
${SYSDATA}: pkg/sysdata/sysdata.R pkg/sysdata/REFERENCES
	cd pkg/sysdata; $R CMD BATCH --vanilla --no-timing sysdata.R
	mv pkg/sysdata/sysdata.rda $@


## auxiliary functions ("canned recipes") for check rules
define CHECK_REPORT_TIMINGS_SCRIPT
timings <- read.table(file.path('surveillance.Rcheck','surveillance-Ex.timings'), header=TRUE, row.names='name')
timings <- timings[order(timings[['elapsed']], decreasing=TRUE),]
cat("Runtimes of examples with elapsed time > 2 seconds:\n\n")
subset(timings, elapsed > 2)
endef
export CHECK_REPORT_TIMINGS_SCRIPT
define check-report-timings
echo "$${CHECK_REPORT_TIMINGS_SCRIPT}" | $R --slave --vanilla
endef

define check-report-warnings-in-examples
cd surveillance.Rcheck; \
nwarn=`grep -c "^Warning" surveillance-Ex.Rout`; \
if [ $$nwarn -gt 0 ]; then echo "\n\tWARNING: $$nwarn" \
	"warning(s) thrown when running examples,\n" \
	"\t         see file surveillance.Rcheck/surveillance-Ex.Rout\n"; fi
endef

## "quick" check
check: build
	_R_CHECK_FORCE_SUGGESTS_=FALSE _R_CHECK_COMPACT_DATA_=FALSE \
	_R_CHECK_PKG_SIZES_=FALSE _R_CHECK_DOC_SIZES_=FALSE \
	$R CMD check --no-manual --ignore-vignettes --check-subdirs=no surveillance_${VERSION}.tar.gz

## standard --as-cran check
check-cran: build-cran
	$R CMD check --as-cran --timings surveillance_${VERSION}.tar.gz
## further option: --use-gct (for better detection of memory bugs/segfaults)
	@$(check-report-timings)
	@$(check-report-warnings-in-examples)

## check with "allExamples" and --run-dontrun
## also use --extra-arch to only do runtime tests (no R and Rd code checking)
## ignore check.Renviron where I set _R_CHECK_LENGTH_1_LOGIC2_=TRUE (stops INLA)
check-allExamples: build
	_R_SURVEILLANCE_ALL_EXAMPLES_=TRUE R_CHECK_ENVIRON="" $R CMD check --timings --run-dontrun --extra-arch --output=/tmp surveillance_${VERSION}.tar.gz
	@$(check-report-timings)
	@$(check-report-warnings-in-examples)


install: build
	$R CMD INSTALL surveillance_${VERSION}.tar.gz

checkUsage: install
	echo "library('surveillance'); library('codetools'); \
	checkUsagePackage('surveillance', suppressFundefMismatch=FALSE, \
	    suppressLocalUnused=TRUE, suppressNoLocalFun=TRUE, skipWith=TRUE, \
	    suppressUndefined=FALSE, suppressPartialMatchArgs=FALSE)" \
	| $R --slave --no-save --no-restore

## we need to run Rd2pdf inside pkg such that \packageTitle finds DESCRIPTION
manual:	
	cd pkg; $R CMD Rd2pdf --batch --force --output=../manual.pdf .

NEWS.html: pkg/inst/NEWS.Rd
	$R --vanilla --slave -e 'tools::Rd2HTML("$<", out = "$@", stylesheet = "http://cran.r-project.org/web/CRAN_web.css")'
	[ `uname -s` = "Darwin" ] && open "$@" || xdg-open "$@"

NEWS_generated_from_HTML.md: NEWS.html
	pandoc -f html-native_spans -t markdown -o "$@" "$<"

www: 
	cd pkg; $R --slave --no-save --no-restore -e \
	  "pkgdown::build_site(examples = FALSE, lazy = TRUE)"


clean:
	make -C pkg/demo clean
	cd pkg/src; rm -f *.o *.so *.dll symbols.rds
	make -C pkg/vignettes clean
	rm -f pkg/*/.Rhistory

.PHONY: build build-cran check check-cran check-allExamples install checkUsage manual www clean

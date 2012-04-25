######################################################################
# Authors: Michael Hoehle, Sebastian Meyer
# History:
#    4 Apr 2012 (SM): R execution command as a variable
#   29 Mar 2012 (SM): Added making of R/sysdata.rda from sysdata/sysdata.R
#    8 Jan 2008 (MH): initial version
#
# Comments: 
# Remove the tar file in the cygwin directory and use the one from Rtools
# watch the list in .Rbuildignore
######################################################################

# Define variable for R bin
# which enables the use of alternatives, e.g., 'make check R=R-devel'
R=R

rd:
	perl-cygwin make.pl Rnw/*.Rnw 

check: clean R/sysdata.rda
	$R CMD check .

install: R/sysdata.rda
	perl-cygwin make.pl --install

build: R/sysdata.rda
#	$R CMD build --force docs=normal --no-vignettes ../surveillance
	$R CMD build --no-vignettes --force .

wininstall: R/sysdata.rda
	R --arch x64 CMD INSTALL .

winbin: R/sysdata.rda
	Rcmd build --binary --no-vignettes --force --docs=normal ../surveillance
#binary:
#	R CMD build --force ../surveillance
##	perl-cygwin make.pl --binary

manual:	
#	perl-cygwin make.pl --manual
	$R CMD Rd2dvi --pdf --no-clean ../surveillance

clean:
	rm -f src/*.o src/*.so; rm -f src/*.dll; rm -f src/symbols.rds

macsrc:
	cd src ; gcc-4.0 -arch i386 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -no-cpp-precomp -I/Library/Frameworks/R.framework/Resources/include -I/Library/Frameworks/R.framework/Resources/include/i386  -msse3  -D__NO_MATH_INLINES  -fPIC  -g -O2 -Wall -pedantic -c surveillance.c -o surveillance.o

#mingw:
#export PATH=/d/Programme/R/R-2.4.0/bin:/d/Programme/Rtools/bin:/d/Programme/MinGW/bin:d:/c/Perl/bin;/c/texmf/miktex/bin
#RCmd build --binary .


# Save internal datasets from ./sysdata/ into R/sysdata.rda
R/sysdata.rda: sysdata/sysdata.R
	cd sysdata; $R CMD BATCH --vanilla --no-timing sysdata.R
	mv sysdata/sysdata.rda $@


######################################################################
# Author: Michael Hoehle
# History:
#   29 Mar 2012: Added making of R/sysdata.rda from develop/sysdata/sysdata.R
#    8 Jan 2008: initial version
#
# Comments: 
# Remove the tar file in the cygwin directory and use the one from Rtools
# watch the list in .Rbuildignore
######################################################################


rd:
	perl-cygwin make.pl Rnw/*.Rnw 

check: clean R/sysdata.rda
	R CMD check .

install: R/sysdata.rda
	perl-cygwin make.pl --install

build: R/sysdata.rda
#	R CMD build --force docs=normal --no-vignettes ../surveillance
	R CMD build --no-vignettes --force .

wininstall: R/sysdata.rda
	R --arch x64 CMD INSTALL .

winbin: R/sysdata.rda
	Rcmd build --binary --no-vignettes --force --docs=normal ../surveillance
#binary:
#	R CMD build --force ../surveillance
##	perl-cygwin make.pl --binary

manual:	
#	perl-cygwin make.pl --manual
	R CMD Rd2dvi --pdf --no-clean ../surveillance

clean:
	rm -f src/*.o src/*.so; rm -f src/*.dll

macsrc:
	cd src ; gcc-4.0 -arch i386 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -no-cpp-precomp -I/Library/Frameworks/R.framework/Resources/include -I/Library/Frameworks/R.framework/Resources/include/i386  -msse3  -D__NO_MATH_INLINES  -fPIC  -g -O2 -Wall -pedantic -c surveillance.c -o surveillance.o

#mingw:
#export PATH=/d/Programme/R/R-2.4.0/bin:/d/Programme/Rtools/bin:/d/Programme/MinGW/bin:d:/c/Perl/bin;/c/texmf/miktex/bin
#RCmd build --binary .


# Save internal datasets from develop/sysdata/ into R/sysdata.rda
R/sysdata.rda: develop/sysdata/sysdata.R
	cd develop/sysdata; R CMD BATCH --vanilla --no-timing sysdata.R
	mv develop/sysdata/sysdata.rda $@


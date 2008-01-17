######################################################################
# Author: Michael Hoehle
# Date:   8 Jan 2008
#
# Comments: 
# Remove the tar file in the cygwin directory and use the one from Rtools
# watch the list in .Rbuildignore
######################################################################

rd:
	perl-cygwin make.pl Rnw/*.Rnw 

check: clean
	R CMD check .

install: 
	perl-cygwin make.pl --install

build:
#	R CMD build --force docs=normal --no-vignettes ../surveillance
	R CMD build --no-vignettes --force .

winbin:
	Rcmd build --binary --no-vignettes --force --docs=normal ../surveillance
#binary:
#	R CMD build --force ../surveillance
##	perl-cygwin make.pl --binary

manual:	
#	perl-cygwin make.pl --manual
	R CMD Rd2dvi --pdf --no-clean ../surveillance

clean:
	rm src/*.o src/*.so rm src/*.dll

macsrc:
	cd src ; gcc-4.0 -arch i386 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -no-cpp-precomp -I/Library/Frameworks/R.framework/Resources/include -I/Library/Frameworks/R.framework/Resources/include/i386  -msse3  -D__NO_MATH_INLINES  -fPIC  -g -O2 -Wall -pedantic -c surveillance.c -o surveillance.o

#mingw:
#export PATH=/d/Programme/R/R-2.4.0/bin:/d/Programme/Rtools/bin:/d/Programme/MinGW/bin:d:/c/Perl/bin;/c/texmf/miktex/bin
#RCmd build --binary .

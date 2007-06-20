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

check:
	R CMD check .

install:
	perl-cygwin make.pl --install

web:
	rsync -avauz surveillance_0.9* ua341cf@sun1.lrz-muenchen.de:~/webserver/webdata/software/surveillance/

build:
	R CMD build --force --docs=normal --no-vignettes ../surveillance

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

#mingw:
#export PATH=/d/Programme/R/R-2.4.0/bin:/d/Programme/Rtools/bin:/d/Programme/MinGW/bin:d:/c/Perl/bin;/c/texmf/miktex/bin
#RCmd build --binary .

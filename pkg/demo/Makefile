## This Makefile defines rules to execute the demo R scripts and
## to compare the current results to previously saved results.
## This Makefile is not part of the R package (it is listed in
## the top-level .Rbuildignore file).
## This Makefile requires GNU make.
##
## Copyright (C) 2016-2017 Sebastian Meyer <seb.meyer@fau.de>
##
## This Makefile can be redistributed and/or modified under the terms
## of the GNU General Public License, version 2,
## a copy of which is available at http://www.R-project.org/Licenses/.

R := R
DEMOSCRIPTS := $(wildcard *.R)
ROUTFILES := ${DEMOSCRIPTS:.R=.Rout}

all : ${ROUTFILES}

%.Rout : %.R
	LANGUAGE=en ${R} CMD BATCH --vanilla --no-timing $< $@
	-mv Rplots.pdf ${<:.R=.pdf}

diff : ${ROUTFILES}
	meld & $(foreach Rout,${ROUTFILES},sleep 2; meld --newtab ${Rout}.save ${Rout} &)
#	$(foreach Rout,${ROUTFILES},${R} CMD Rdiff ${Rout} ${Rout}.save;)

clean :
	rm -f ${DEMOSCRIPTS:.R=.pdf} ${DEMOSCRIPTS:.R=.Rout}

.PHONY: all clean diff

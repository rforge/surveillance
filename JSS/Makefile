MASTER = surveillance

BIBFILE  = references.bib
TEXFILES = header.tex abstract.tex
RNWFILES = $(wildcard *.Rnw)
DEPENDS  = ${TEXFILES} $(patsubst %.Rnw,%.tex,${RNWFILES}) ${BIBFILE}

R = R
ROPTS = --slave --no-save --no-restore
RSETUPFILE = setup.R

.PHONY: clean

${MASTER}.pdf:	${MASTER}.tex ${DEPENDS}
	texi2pdf --batch $<

%.tex: %.Rnw ${RSETUPFILE}
	echo "library('knitr'); knit('$<')" | ${R} ${ROPTS}

clean:
	rm -f *.dvi *.log *.toc *.bmt *.blg *.idx *.ilg *.ind *.mtc* *.flc *.out *.nav *.snm *.vrb *.bbl *.aux

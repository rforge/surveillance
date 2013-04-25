MASTER = surveillance

BIBFILE  = references.bib
TEXFILES = header.tex abstract.tex
RNWFILES = $(wildcard *.Rnw)
DEPENDS  = ${TEXFILES} $(patsubst %.Rnw,%.tex,${RNWFILES}) ${BIBFILE}

R = R
ROPTS = --slave --no-save --no-restore

.PHONY: clean

${MASTER}.pdf:	${MASTER}.tex ${DEPENDS}
	texi2pdf --batch --quiet $<

%.tex: %.Rnw setup.R
	echo "library('knitr'); knit('$<')" | ${R} ${ROPTS}
	
clean:
	rm -f *.dvi *.log *.toc *.bmt *.bbl *.blg *.idx *.ilg *.ind *.mtc* *.flc *.out *.nav *.snm *.vrb #*.aux

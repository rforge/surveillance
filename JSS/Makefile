MASTER := surveillance
TEXFILES := header.tex abstract.tex
RNWFILES := $(wildcard *.Rnw)
BIBFILES := $(wildcard references*.bib)
PDFDEPENDS := ${TEXFILES} $(patsubst %.Rnw,%.tex,${RNWFILES}) ${BIBFILES}

# for Sebastian's site (full literature DB file)
BIBDB := literatur.bib

# R settings
R := R
ROPTS := --slave --no-save --no-restore
RSETUPFILE := setup.R

${MASTER}.pdf: ${MASTER}.tex ${PDFDEPENDS}
	@cp -a ${MASTER}.aux ${MASTER}.aux~ # archive original aux-file
	texi2pdf --batch $<
# if new aux-file equals old one, then restore the backup (-> old timestamp)
	@cmp -s ${MASTER}.aux ${MASTER}.aux~; if [ $$? -eq 0 ]; then \
		mv ${MASTER}.aux~ ${MASTER}.aux; else rm ${MASTER}.aux~; fi

%.tex: %.Rnw ${RSETUPFILE}
	@echo "library('knitr'); knit('$<')" | ${R} ${ROPTS}

# automatically generate subdatabase of BIBDB according to aux-file
references_sebastian.bib: ${MASTER}.aux
	@if [ -s ${BIBDB} ]; then \
		jabref --nogui --aux=$<,$@ ${BIBDB}; \
		sed -i "1i% !!! THIS IS AN AUTO-GENERATED FILE !!!" $@; \
		rm -f $@.bak; \
	else \
		echo Note: ${BIBDB} not available, only touching $@; \
		touch $@; \
	fi

# if there is no aux-file we need to generate one first
${MASTER}.aux:
	pdflatex -interaction=nonstopmode ${MASTER}.tex

clean:
	rm -f *.dvi *.log *.toc *.bmt *.blg *.idx *.ilg *.ind *.mtc* *.flc *.out *.nav *.snm *.vrb *.bbl #*.aux

.PHONY: clean

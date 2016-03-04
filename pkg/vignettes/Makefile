## This is a convenient Makefile for quick vignette compilation in Emacs
## and for author-level checks of the vignette cache. It requires GNU make.
## Because of this and because this Makefile does nothing special to build
## the vignettes, it is listed in the top-level .Rbuildignore file
## (such that standard R CMD build routines take care of the vignettes).
## In addition it defines a rule to create the PStricks-based HMM figure
## included in surveillance.Rnw
##
## Copyright (C) 2016  Sebastian Meyer <Sebastian.Meyer@ifspm.uzh.ch>
## This Makefile can be redistributed and/or modified under the terms
## of the GNU General Public License, version 2 or later.

R := R
RNWFILES := $(wildcard *.Rnw)
TEXFILESFROMRNW := ${RNWFILES:.Rnw=.tex}
VIGNETTES := ${RNWFILES:.Rnw=.pdf}
CACHEFILES := $(wildcard $(patsubst %.Rnw,%-cache.RData,${RNWFILES}))

.PHONY: all
all: ${VIGNETTES}

%.pdf: %.tex references.bib
	${R} --vanilla --slave -e "tools::texi2pdf('$<', clean = TRUE)"

.SECONDARY: ${TEXFILESFROMRNW}  # make shall not remove intermediate tex files
%.tex: %.Rnw
	${R} CMD Sweave "$<"

.PHONY: clean cleanall
clean:
	rm -rf *-R.bib plots/ $(addprefix *-cache.RData,.new .diff)
cleanall: clean
	rm -rf ${VIGNETTES} ${TEXFILESFROMRNW}


### create surveillance-hmm.pdf

surveillance.pdf: surveillance-hmm.pdf  # additional dependency
surveillance-hmm.pdf: surveillance-hmm.tex
	latex -interaction=batchmode $< && dvips -q ${<:.tex=.dvi} && \
		ps2pdf -q -sOutputFile=- ${<:.tex=.ps} | pdfcrop - $@
	rm -f $(addprefix $(basename $<).,aux dvi log ps)


### rebuild cache and check if it differs from the old one

.PHONY: check-cache
check-cache: $(addsuffix .diff,${CACHEFILES})

$(addsuffix .diff,${CACHEFILES}): %.diff: % %.new
	@echo CHECKING $<.new AGAINST $< ...
	@${R} --vanilla --slave -e "\
old <- attach('$<'); \
new <- attach('$<.new', warn.conflicts = FALSE); \
equal <- all.equal(old, new); \
if (isTRUE(equal)) message('OK: $< needs no update') else { \
message('CAVE: $< MAY NEED TO BE UPDATED\n-> $<.diff'); print(equal) }" > "$@"

$(addsuffix .new,${CACHEFILES}): %.new: %
	@echo REBUILDING $< AS $@ ...
	mv "$<" "$<.bak"
	-${MAKE} -B ${<:-cache.RData=.tex}
	-mv "$<" "$@"
	mv "$<.bak" "$<"

nothing:
	echo "Doing nothing, to compile the vignette use make vignette" 

vignette: sweave
	pdflatex vignette.tex
	bibtex vignette
	pdflatex vignette.tex
	pdflatex vignette.tex
	rm vignette.blg vignette.log vignette.out vignette.aux vignette.bbl 

sweave:
	echo "Sweave(\"vignette.Rnw\")" | R --no-save --no-restore
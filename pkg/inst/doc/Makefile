HHH=vignette_hhh4

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

hhh4:
	echo "Sweave(\"$(HHH).Rnw\")" | R --no-save --no-restore
	pdflatex $(HHH)
	bibtex $(HHH)
	pdflatex $(HHH)
	pdflatex $(HHH)
	rm *~ $(HHH).aux $(HHH).bbl $(HHH).blg $(HHH).log $(HHH).out Rplots.pdf

showHHH:
	acroread $(HHH).pdf & 


VIGNETTE=vignette_hhh4

all: sweave latex clean

fast: sweave
	pdflatex $(VIGNETTE)
	acroread $(VIGNETTE).pdf & 
	
sweave: 
	R CMD Sweave $(VIGNETTE).Rnw
	
latex:
	pdflatex $(VIGNETTE)
	bibtex $(VIGNETTE)
	pdflatex $(VIGNETTE)
	pdflatex $(VIGNETTE)
	acroread $(VIGNETTE).pdf & 

clean:
	rm *~ *.aux *.bbl *.blg *.log *.out Rplots.pdf
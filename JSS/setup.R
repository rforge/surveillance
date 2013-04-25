opts_chunk$set(echo=FALSE, results='markup', fig.path='plots/R-',
               out.width='.49\\linewidth', fig.width=6, fig.height=6,
               cache=TRUE, cache.path='.cache/')
render_sweave() # use Sweave environments
set_header(highlight = '') # do not use the Sweave.sty package
pdf.options(family = 'Palatino')
options(prompt="R> ", continue="+  ", width=70, useFancyQuotes=FALSE)   # JSS
options(digits=3, xtable.booktabs = TRUE)
set.seed(1613)  # for reproducibility

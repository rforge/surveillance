
## knitr stuff
library("knitr")
opts_chunk$set(echo=TRUE, results='markup', fig.path='plots/R-',
               out.width='.49\\linewidth', fig.width=6, fig.height=6,
               fig.align="center", cache=TRUE, cache.path='.cache/')
render_sweave()  # use Sweave environments
set_header(highlight = '')  # do not use the Sweave.sty package

## R settings
pdf.options(family = 'Palatino')
options(prompt="R> ", continue="+  ", width=70, useFancyQuotes=FALSE)  # JSS
options(digits=3, xtable.booktabs=TRUE)
options(scipen=1)  # so that 1e-4 gets still printed as 0.0001

## Further packages
library("lattice")
# Set default plot order for lattice graphics
local({
    latticeDefaults <- lattice.getOption("default.args")
    latticeDefaults$as.table <- TRUE   # go left to right, top to bottom by default
    lattice.options(default.args = latticeDefaults)
})
library("surveillance")

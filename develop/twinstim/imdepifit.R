################################################################################
### Author: Sebastian Meyer [seb *.* meyer *a*t* fau *.* de]
### Time-stamp: <[imdepifit.R] 2019-07-19 17:07 (CEST) by SM>
### Project: reproduce data(imdepifit)
################################################################################

library("surveillance")
data("imdepi", "imdepifit")

myimdepifit <- twinstim(
    endemic = addSeason2formula(~ offset(log(popdensity)) + I(start/365-3.5),
                                S = 1, period = 365, timevar = "start"),
    epidemic = ~ type + agegrp, siaf = siaf.gaussian(),
    data = imdepi, subset = !is.na(agegrp),
    optim.args = list(control = list(reltol = sqrt(.Machine$double.eps))),
                      ## par = c(-20, 0, 0.2, 0.3, -12, -1, 0, 0, 3)),
                      ## CAVE: start values affect estimates (tolerance ~0.002)
    model = FALSE, cumCIF = FALSE
)

## compare with the one stored in the package
all.equal(imdepifit, myimdepifit)
## only the "runtime" component should be different!
rbind(imdepifit$runtime, myimdepifit$runtime)

if (FALSE) # store updated fit in data-folder
{
    imdepifit <- myimdepifit
    save(imdepifit, file="~/Projekte/surveillance/pkg/data/imdepifit.RData", version = 2)
    tools::resaveRdaFiles("~/Projekte/surveillance/pkg/data/imdepifit.RData")
}

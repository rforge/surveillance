################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[imdepifit.R] by SM Don 29/08/2013 17:13 (CEST)>
### Project: reproduce data(imdepifit)
################################################################################


### data(imdepifit) is obtained as

library("surveillance")
data("imdepi")

myimdepifit <- twinstim(
    endemic = addSeason2formula(~ offset(log(popdensity)) + I(start/365-3.5),
                                S=1, period=365, timevar="start"),
    epidemic = ~ type + agegrp, siaf = siaf.gaussian(1, F.adaptive=TRUE),
    data = imdepi, subset = !is.na(agegrp),
    control.siaf = list(F=list(adapt=0.25), Deriv=list(nGQ=13)), 
    optim.args = list(par = c(-20, -0.1, 0.2, 0.3, -12, -1, 0, 0, 3),
                      control = list(trace=1)),
    model = FALSE, cumCIF = TRUE
)

## compare with the one stored in the package
data("imdepifit")
all.equal(imdepifit, myimdepifit)
## only the "runtime" component should be different!

if (FALSE) # store updated fit in data-folder
{
    imdepifit <- myimdepifit
    save(imdepifit, file="~/Projekte/surveillance/pkg/data/imdepifit.RData")
    tools::resaveRdaFiles("~/Projekte/surveillance/pkg/data/imdepifit.RData")
}

################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[imdepifit.R] by SM Mon 31/03/2014 17:36 (CEST)>
### Project: reproduce data(imdepifit)
################################################################################


### data(imdepifit) is obtained as

library("surveillance")
data("imdepi")

myimdepifit <- twinstim(
    endemic = addSeason2formula(~ offset(log(popdensity)) + I(start/365-3.5),
                                S=1, period=365, timevar="start"),
    epidemic = ~ type + agegrp,
    siaf = siaf.gaussian(nTypes=1, F.adaptive=TRUE),
    data = imdepi, subset = !is.na(agegrp),
    control.siaf = list(F=list(adapt=0.25), Deriv=list(nGQ=13)), 
    optim.args = list(par = c(-20, 0, 0.2, 0.3, -12, -1, 0, 0, 3)),
    model = FALSE, cumCIF = TRUE
)

## compare with the one stored in the package
data("imdepifit")
all.equal(imdepifit, myimdepifit)
## only the "runtime" component should be different!
rbind(imdepifit$runtime, myimdepifit$runtime)

if (FALSE) # store updated fit in data-folder
{
    imdepifit <- myimdepifit
    save(imdepifit, file="~/Projekte/surveillance/pkg/data/imdepifit.RData")
    tools::resaveRdaFiles("~/Projekte/surveillance/pkg/data/imdepifit.RData")
}

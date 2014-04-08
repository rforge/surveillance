################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[imdepifit.R] by SM Die 08/04/2014 18:23 (CEST)>
### Project: reproduce data(imdepifit), should be as in the JSS paper
################################################################################


### data(imdepifit) is obtained as

library("surveillance")
data("imdepi")

imdepifit_endemic <- twinstim(
    endemic = addSeason2formula(~ offset(log(popdensity)) + I(start/365-3.5),
                                S=1, period=365, timevar="start"),
    data = imdepi, subset = !is.na(agegrp))

imdepifit <- update(imdepifit_endemic,
    epidemic = ~ type + agegrp, siaf = siaf.gaussian(),
    control.siaf = list(F=list(adapt=0.25), Deriv=list(nGQ=13)),
    start = list(epidemic=c("(Intercept)"=-12.5, "typeC"=-1, "siaf.1"=2.8)))


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

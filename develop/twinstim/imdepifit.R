################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[imdepifit.R] by SM Die 08/04/2014 23:22 (CEST)>
### Project: reproduce data(imdepifit) as in the JSS paper (two-stage procedure)
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
    start = list(epidemic=c("(Intercept)"=-12.5, "typeC"=-1, "siaf.1"=2.7)))

## compare with the one stored in the package
imdepifit_installed <- local({data("imdepifit", envir=environment()); imdepifit})
all.equal(imdepifit_installed, imdepifit)
## only the "runtime" component should be different!
rbind(imdepifit_installed$runtime, imdepifit$runtime)

if (FALSE) # store updated fit in data-folder
{
    save(imdepifit, file="~/Projekte/surveillance/pkg/data/imdepifit.RData")
    tools::resaveRdaFiles("~/Projekte/surveillance/pkg/data/imdepifit.RData")
}

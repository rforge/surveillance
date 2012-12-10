################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[imdepifit.R] by SM Mon 10/12/2012 12:42 (CET)>
### Project: reproduce data(imdepifit)
################################################################################


### data(imdepifit) is obtained as

data(imdepi)
data(imdepifit)

myimdepifit <- twinstim(
    endemic = ~1 + offset(log(popdensity)) + I(start/365) + 
        sin(start * 2 * pi/365) + cos(start * 2 * pi/365),
    epidemic = ~1 + type + agegrp, siaf = siaf.gaussian(1, F.adaptive=TRUE),
    data = imdepi, subset = !is.na(agegrp),
    control.siaf = list(F=list(adapt=0.25), Deriv=list(nGQ=13)), 
    optim.args = list(par = c(-20, -0.041, 0.24, 0.35, -17, 0, 0, 0, 4)),
    # initial values obtained from model with epidemic=~1 and constant siaf
    model = FALSE, cumCIF = TRUE
)

all.equal(imdepifit, myimdepifit)
## only the "runtime" component should be different!


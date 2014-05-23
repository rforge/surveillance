################################################################################
## Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
## Time-stamp: <[glmfit.R] by SM Fre 23/05/2014 11:30 (CEST)>
## Project: Reproduce endemic-only twinstim fit by an equivalent Poisson-GLM fit
################################################################################


library("surveillance")

# Load invasive meningococcal disease data
data("imdepi", package="surveillance")


### first, fit an endemic-only model

m_noepi <- twinstim(
    endemic = ~ 1 + offset(log(popdensity)) + I(start/365) +
                sin(start * 2 * pi/365) + cos(start * 2 * pi/365),
    data = imdepi, subset = !is.na(agegrp),
    model = FALSE, cumCIF = FALSE   # for reasons of speed
)

## look at the model summary
summary(m_noepi)



### this endemic-only twinstim equals a Poisson-GLM on the observations in
### stgrid with outcome being the number of events by grid cell

mystgrid <- imdepi$stgrid
eventsbycell <- c(table(with(imdepi$events@data,
    interaction(tile, BLOCK, drop=FALSE, sep=".", lex.order=FALSE))))
mystgrid$nEvents <- eventsbycell[paste(mystgrid$tile, mystgrid$BLOCK, sep=".")]
all.equal(mystgrid$nEvents, unname(eventsbycell))  # actually don't need above indexing
stopifnot(sum(mystgrid$nEvents) == nrow(imdepi$events))

myglm <- glm(update(formula(m_noepi)$endemic, nEvents ~ .),
             data=mystgrid, family=poisson)
summary(myglm)
## but the intercept is not correct


### need an additional offset: log(nTypes*ds*dt)

nTypes <- nlevels(imdepi$events$type)
myglmoffset <- glm(update(formula(m_noepi)$endemic, nEvents ~ .),
                   data=mystgrid, family=poisson,
                   offset=log(nTypes * (stop-start) * area))
summary(myglmoffset)
# estimates and standard errors reproduced (apart from numerical differences)


## if we had a type-specific model, we would have to set up a "stkappagrid",
## i.e. with nBlocks * nTiles * nTypes rows...

m_noepi_types <- update(m_noepi, endemic=~(1|type) + ., optim.args=list(par=c(0,coef(m_noepi))))
summary(m_noepi_types)

mystkappagrid <- rbind(cbind(imdepi$stgrid,type="B"), cbind(imdepi$stgrid,type="C"))
eventsbycell <- c(table(with(imdepi$events@data,
    interaction(tile, BLOCK, type, drop=FALSE, sep=".", lex.order=FALSE))))
mystkappagrid$nEvents <- eventsbycell[with(mystkappagrid, paste(tile,BLOCK,type,sep="."))]
all.equal(mystkappagrid$nEvents, unname(eventsbycell))  # actually don't need above indexing
stopifnot(sum(mystkappagrid$nEvents) == nrow(imdepi$events))

myglmtypes <- glm(update(formula(m_noepi)$endemic, nEvents ~ type + . - 1),
                  data=mystkappagrid, family=poisson,
                  offset=log((stop-start) * area))
summary(myglmtypes)
# in accordance with the results from the corresponding twinstim-fit ! :)



### try to fit equivalent model with twinSIR

data("imdepi")
imdepi_short <- subset(imdepi, time < 200)
imdepi_short$stgrid <- subset(imdepi_short$stgrid, start < 200)

## endemic-only twinstim()
data("imdepifit")
form <- ~ log(popdensity) + I(start/365)
fit_twinstim <- update(imdepifit, endemic = form, epidemic = ~0, siaf = NULL,
                       data = imdepi_short, model=TRUE,
                       optim.args = list(control=list(trace=0)), verbose=FALSE)
fit_twinstim

## convert imdepi_short to "epidata" by districts
load(system.file("shapes", "districtsD.RData", package="surveillance"))
imdepi_short_epidata <- as.epidata(imdepi_short,
                                   tileCentroids=coordinates(districtsD))
fit_twinSIR <- twinSIR(~cox(log(popdensity)) + cox(I(start/365)),
                       data=imdepi_short_epidata)
fit_twinSIR
## this is something different since we look at infectivity of districts as a
## whole disregarding the number of infective individuals within the tiles.
## we also have a different grid in time (due to extra stop times at recovery).

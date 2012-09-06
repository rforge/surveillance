################################################################################
## Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
## Time-stamp: <[glminit.R] by SM Mon 03/09/2012 12:39 (CEST)>
## Project: Reproduce endemic-only twinstim fit by an equivalent Poisson-GLM fit
################################################################################


library("surveillance")


### from example(twinstim):

# Load invasive meningococcal disease data
data("imdepi")

### first, fit an endemic-only model

## As a start value for the endemic intercept use the crude estimate
## assuming the model only had a single-cell endemic component
## (rate of homogeneous Poisson process scaled for population density)
popdensity.overall <- with(subset(imdepi$stgrid, BLOCK == 1),
    weighted.mean(popdensity, area))
popdensity.overall   # pop. density in Germany is ~230 inhabitants/km^2
W.area <- with(subset(imdepi$stgrid, BLOCK == 1), sum(area))
W.area               # area of Germany is about 357100 km^2
# this should actually be the same as
sum(sapply(imdepi$W@polygons, slot, "area"))
# which here is not exactly the case because of polygon simplification

## start value for the endemic intercept
h.intercept <- with(summary(imdepi),
    log(nEvents/popdensity.overall/diff(timeRange)/W.area))

## fit the endemic-only model
m_noepi <- twinstim(
    endemic = ~ 1 + offset(log(popdensity)) + I(start/365) +
                sin(start * 2 * pi/365) + cos(start * 2 * pi/365),
    data = imdepi, subset = !is.na(agegrp),
    optim.args = list(par=c(h.intercept,rep(0,3)),
                      method="nlminb", control = list(REPORT=1)),
    model = FALSE, cumCIF = FALSE   # for reasons of speed
)

## look at the model summary
summary(m_noepi)



### doesn't this equal a Poisson-GLM on the observations in stgrid with outcome
### being the number of events by grid cell?

mystgrid <- imdepi$stgrid
eventsbycell <- c(table(with(imdepi$events@data,
    interaction(tile, BLOCK, drop=FALSE, sep=".", lex.order=FALSE))))
mystgrid$nEvents <- eventsbycell[paste(mystgrid$tile, mystgrid$BLOCK, sep=".")]
all.equal(mystgrid$nEvents, unname(eventsbycell))  # actually don't need above indexing
stopifnot(sum(mystgrid$nEvents) == nrow(imdepi$events))

myglm <- glm(update(formula(m_noepi)$endemic, nEvents ~ .),
             data=mystgrid, family=poisson)
summary(myglm)
## the intercept is not correct here


## the correct solution is a weighted Poisson-GLM with pseudo-observations
## nEvents / weights, and weights = K * dt * ds

nTypes <- nlevels(imdepi$events$type)
mystgrid$weights <- nTypes * (mystgrid$stop-mystgrid$start) * mystgrid$area
myglmweights <- glm(update(formula(m_noepi)$endemic, nEvents/weights ~ .),
                    data=mystgrid, family=poisson, weights=weights)
# gives warnings because of non-integer pseudo-outcome
summary(myglmweights)
# estimates and standard errors are reproduced (apart from numerical differences)


## if we had a type-specific model, we would have to set up a "stkappagrid",
## i.e. with nBlocks * nTiles * nTypes rows... (and remove the factor nTypes 
## from the weights)

m_noepi_types <- update(m_noepi, endemic=~(1|type) + ., optim.args=list(par=c(0,coef(m_noepi))))
summary(m_noepi_types)

nTypes <- nlevels(imdepi$events$type)
mystkappagrid <- rbind(cbind(imdepi$stgrid,type="B"), cbind(imdepi$stgrid,type="C"))
mystkappagrid$weights <- (mystkappagrid$stop-mystgrid$start) * mystkappagrid$area
eventsbycell <- c(table(with(imdepi$events@data,
    interaction(tile, BLOCK, type, drop=FALSE, sep=".", lex.order=FALSE))))
mystkappagrid$nEvents <- eventsbycell[with(mystkappagrid, paste(tile,BLOCK,type,sep="."))]
all.equal(mystkappagrid$nEvents, unname(eventsbycell))  # actually don't need above indexing
stopifnot(sum(mystkappagrid$nEvents) == nrow(imdepi$events))

myglmtypes <- glm(update(formula(m_noepi)$endemic, nEvents/weights ~ type + . - 1),
                  data=mystkappagrid, family=poisson, weights=weights)
summary(myglmtypes)
# in accordance with the results from the corresponding twinstim-fit ! :)


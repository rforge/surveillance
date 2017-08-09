
### epidataCS simulation

## example tiles
data("fluBYBW")
franconia <- fluBYBW@map[substr(row.names(fluBYBW@map), 1, 2) %in% c("94","95","96"),]
franconia@data <- franconia@data["name"]

## stgrid template
breaks <- seq(0, by = 30, length.out = 13)
stgrid <- tiles2stgrid(franconia, start = breaks[-13], T = breaks[13])

## beta0 for 10 endemic events (cf. surveillance:::crudebeta0)
log(10/areaSpatialPolygons(franconia)/diff(range(breaks)))

## expected number of offspring (subcritical)
exp(-9.5) * siaf.gaussian()$Fcircle(1e5, 3) * 5

## simulated epidemic
set.seed(83)
myepi <- simEpidataCS(endemic = ~1, epidemic = ~1,
                      siaf = siaf.gaussian(F.adaptive=FALSE, F.method="iso"),
                      rmarks = function (t, s) data.frame(eps.s = Inf, eps.t = 5),
                      stgrid = stgrid, tiles = franconia,
                      beta0 = -18, gamma = -9.5, siafpars = 3,
                      trace = 10, nEvents = 600)

plot(myepi)
plot(myepi, "space")
plot(as.stepfun(myepi))
##animate(myepi)

mean(myepi$events$source == 0)  # empirical endemic proportion


### fit the true model

fit0 <- twinstim(endemic = ~1, epidemic = ~1,
                 siaf = siaf.gaussian(F.adaptive = FALSE, F.method = "iso"),
                 data = myepi, model = TRUE)
cbind(true = coef(myepi), estimated = coef(fit0))

plot(fit0, "endemic")
plot(fit0, "epidemic", "space", tiles = franconia, sgrid = 5000)

plot(fit0, "siaf", xlim = c(0, 100), ylim = c(0, 1e-4))
true_scaled_siaf <- function (x)
    exp(coef(myepi)["e.(Intercept)"]) *
        myepi$formula$siaf$f(cbind(x, 0), pars = coef(myepi)["e.siaf.1"])
curve(true_scaled_siaf(x), add = TRUE, lwd = 3)


### fit alternative siafs


fit_step <- update(fit0, siaf = quantile(getSourceDists(myepi), c(10,20,40)/100))
AIC(fit0, fit_step)
plot(fit_step, "siaf", add = TRUE, col.estimate = 4)

## visually track the fitting process
vtrace <- function (pars) {
    with(parent.frame(), # loglik() call
         curve(exp(gamma[[1]])*siaf$f(cbind(x,0), siafpars),
               add = TRUE, col = rgb(0,0,0,0.2)))
    TRUE
}

fit1 <- update(fit0, siaf = siaf.student(validpars=vtrace))  # bad convergence, gamma0 -> Inf

curve(true_scaled_siaf(x), 0, 50, ylim = c(0, 2e-4), lwd = 3)
fit1 <- update(fit0, siaf = siaf.powerlaw(validpars=vtrace))  # non-identifiable, gamma0 -> Inf

##fit1 <- update(fit0, siaf = siaf.powerlaw(), optim.args = list(method = "Nelder-Mead"))

## fixing sigma = 1 allows convergence
fit1 <- update(fit0, siaf = siaf.powerlaw(validpars=vtrace),
               start = c("e.siaf.1" = 0), optim.args=list(fixed = "e.siaf.1"))
summary(fit1)  # CAVE: e.siaf.1 fixed!
fit1b <- update(fit0, siaf = siaf.powerlaw(validpars=vtrace),
                start = c("e.siaf.1" = log(min(getSourceDists(myepi)))), optim.args=list(fixed = "e.siaf.1"))
fit1c <- update(fit0, siaf = siaf.powerlaw(validpars=vtrace),
                start = c("e.siaf.1" = log(mean(getSourceDists(myepi)))), optim.args=list(fixed = "e.siaf.1"))
curve(true_scaled_siaf(x), 0, 50, ylim = c(0, 2e-4), lwd = 3)
plot(fit1, "siaf", add = TRUE, col.estimate = 2)
plot(fit1b, "siaf", add = TRUE, col.estimate = 2)
plot(fit1c, "siaf", add = TRUE, col.estimate = 2)



## different parametrization f(x)=(1+x/sigma)^-d
curve(true_scaled_siaf(x), 0, 50, ylim = c(0, 2e-4), lwd = 3)
fit2 <- update(fit0, siaf = siaf.powerlaw2(validpars=vtrace),
               start = c("e.(Intercept)" = -5, "e.siaf.2" = 0.5),
               control.siaf=list(Deriv=list(nGQ=30)))
## also not identifiable (now sigma and d grow simultaneously...)

fit2 <- update(fit0, siaf = siaf.powerlaw2(validpars=vtrace),
               start = c("e.siaf.1" = log(mean(getSourceDists(myepi))), "e.siaf.2" = 0.5), optim.args=list(fixed = "e.siaf.1"),
               control.siaf=list(Deriv=list(nGQ=35)))
## more efficient parametrization than fit1c (where gamma0 becomes quiet large)

## lagged power law
fit3 <- update(fit0, siaf = siaf.powerlawL())    # proper convergence !
AIC(fit0, fit_step, fit3)
plot(fit3, "siaf", add = TRUE, col.estimate = 3)



### what if we generate the epidemic from a powerlaw ?

exp(-5) * siaf.powerlaw()$Fcircle(1000, c(0,0.75)) * 5

set.seed(3)
myepiPL <- simEpidataCS(endemic = ~1, epidemic = ~1, siaf = siaf.powerlaw(),
                        rmarks = function (t, s) data.frame(eps.s = Inf, eps.t = 5),
                        stgrid = stgrid, tiles = franconia,
                        beta0 = -18, gamma = -5.05, siafpars = c(0,0.7),
                        trace = 10, nEvents = 600)

plot(as.stepfun(myepiPL))
plot(myepiPL, "space", by = source==0)

fit0PL <- twinstim(endemic = ~1, epidemic = ~1, siaf = siaf.powerlaw(),
                   data = myepiPL, model = TRUE)
cbind(true = coef(myepiPL), estimated = coef(fit0PL))
summary(fit0PL)

plot(fit0PL, "siaf", xlim = c(0, 30))
true_scaled_siaf <- function (x)
    exp(coef(myepiPL)[2]) * myepiPL$formula$siaf$f(cbind(x, 0), coef(myepiPL)[3:4])
curve(true_scaled_siaf(x), lwd = 3, add = TRUE)

fit1PL <- update(fit0PL, siaf = siaf.student())
plot(fit1PL, "siaf", add = TRUE, col.estimate = 4)

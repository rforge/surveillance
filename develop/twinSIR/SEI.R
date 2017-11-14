################################################################################
### Author: Sebastian Meyer [seb *.* meyer *a*t* fau *.* de]
### Time-stamp: <[SEI.R] 2017-11-14 12:24 (CET) by SM>
### Project: Bug report on tE.col by Caterina de Bacco (e-mail, 2017-11-14)
###
### In surveillance <= 1.15.0, specifying latent periods ('tE.col') was actually
### not possible since as.epidata.default() checked that events (I) can only
### happen when at risk (S). This check can now be disabled (with a warning).
################################################################################

devtools::load_all("~/Projekte/surveillance/pkg")
##library("surveillance")

## minimal example using the hagelloch data
data("hagelloch")
hagelloch.df <- hagelloch.df[order(hagelloch.df$tI),]
head(hagelloch.df)
seir <- hagelloch.df[1:4, c("PN", "tI", "tR", "x.loc", "y.loc")]
set.seed(1)
seir$tE <- seir$tI - rnorm(nrow(seir), mean = 9, sd = 2)
seir

##debugonce(as.epidata.data.frame)
myepi <- as.epidata(seir, t0 = 0, tE.col = "tE", tI.col = "tI", tR.col = "tR",
                    id.col = "PN", coords.cols = c("x.loc", "y.loc"))

##Error in as.epidata.default(data = evHist, id.col = "id", start.col = "start",  :
##  inconsistent atRiskY/event indicators in row 6: event only if at risk

summary(myepi)  ## BUG: only 184 is initially infectious
## FIXME: need to be able to overwrite default atRiskY==0 interpretation for
##        initially infectious in as.epidata.default, update.epidata, [.epidata
##        (and potentially other functions)

summary(myepi)$counters  ## BUG: nS and nI are wrong
plot(myepi)  ## uses "counters" so is also wrong

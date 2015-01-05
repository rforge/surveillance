#Test linelist2sts
library("surveillance")

dates <- seq(as.Date("2001-12-01"),as.Date("2014-12-31"),by="1 day")
t <- seq_len(length(dates))

sts <- disProg2sts(sim.pointSource(p = 0.995, r = 0.1, length = length(t),
                                   A = 1, alpha = 1, beta = 0, phi = 0,
                                   frequency = 1, state = NULL, K = 2))
epoch(sts) <- as.numeric(dates)
sts@epochAsDate <- TRUE

#plot(sts)

D <- data.frame(tEvent=rep(dates,observed(sts)))

sts.day <- linelist2sts(D,dateCol="tEvent",aggregate.by="1 day")
sts.week <- linelist2sts(D,dateCol="tEvent",aggregate.by="1 week")
sts.month <- linelist2sts(D,dateCol="tEvent",aggregate.by="1 month")
sts.quarter <- linelist2sts(D,dateCol="tEvent",aggregate.by="3 month")
sts.year <- linelist2sts(D,dateCol="tEvent",aggregate.by="1 year")

plot(sts.day,legend.opts=NULL)
lines(observed(sts),col="green")

plot(sts.week,legend.opts=NULL)
epoch(sts.week)
all.equal(as.numeric(format(epoch(sts.week),"%u")),rep(1L,nrow(sts.week)))

plot(sts.month,legend.opts=NULL)
epoch(sts.month)
all.equal(as.numeric(format(epoch(sts.month),"%d")),rep(1L,nrow(sts.month)))

plot(sts.quarter,legend.opts=NULL)
plot(sts.quarter,xaxis.tickFreq=list("%Q"=atChange),
     xaxis.labelFreq=list("%Q"=atChange),xaxis.labelFormat="Q%Q-%Y",
     xlab="Time (quarters)",lty=c(1,1,1,1),lwd=c(1,1,2),legend.opts=NULL)


plot(sts.year,legend.opts=NULL)
plot(sts.year,xaxis.tickFreq=list("%Y"=atChange),
     xaxis.labelFreq=list("%Y"=atChange),xaxis.labelFormat="%Y",
     xlab="Time (years)",lty=c(1,1,1,1),lwd=c(1,1,2),legend.opts=NULL)

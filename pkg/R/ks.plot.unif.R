#################
# Plot the empirical distribution function of a sample from U(0,1)
# together with a confidence band of the corresponding K-S-test
#
# Parameters:
#    U - numeric vector containing the sample
#    conf.level - confindence level for the K-S-test, can also be a vector of multiple levels
#    col.conf - colour of the confidence band
#    col.ref - colour of the reference line
#################

ks.plot.unif <- function (U, conf.level = 0.95,
	col.conf = "gray", col.ref = "gray",
	xlab = expression(u[(i)]), ylab = "Cumulative distribution")
{
    n <- length(U)
    
	#Helper function to invert the K-S test
	#pkolmogorov2x is the CDF of the Kolmogorov test statistic
	f <- function(x,p) {
		1 - .C("pkolmogorov2x", p = as.double(x), as.integer(n), PACKAGE = "stats")$p - p
	}

	#Test inversion
	Dconf <- sapply(conf.level, function (level) uniroot(f, lower=0, upper=1, p=1-level)$root)

	#Small helper function to draw a line
    myabline <- function(a, b, x.grid = seq(0,1,length.out=101), ...) {
      lines(x.grid, a + b * x.grid, ...)
    }
    
    #Figure 10 in Ogata (1988)
    plot(c(0,1), c(0,1), type="n", xlab=xlab, ylab=ylab)
    myabline(a=0,b=1,col=col.ref,lwd=2)
    rug(U)
    lines(U, ecdf(U)(U), type="s")
    sapply(Dconf, function (D) {
    	myabline(a=D,b=1,col=col.conf,lty=2)
	    myabline(a=-D,b=1,col=col.conf,lty=2)
    })
    #legend(x="topleft",lty=2,col=col,legend=paste(100*conf.level,"% KS error bounds", sep=""))
}


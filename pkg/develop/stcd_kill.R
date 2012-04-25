### problem solved (was again related to use of "long"-type in C-code)
##############

library("surveillance")
library("splancs")
data(burkitt)


examplestcd <- function (seed)
{
     set.seed(seed)
     
     # order the times
     burkitt <- burkitt[order(burkitt$t), ]
     
     #Parameters for the SR detection
     epsilon <- 0.5 # relative change within the cluster
     radius <- 20 # radius
     threshold <- 161 # threshold limit
     
     res <- stcd(x=burkitt$x,
                 y=burkitt$y,
                 t=burkitt$t,
                 radius=radius,
                 epsilon=epsilon,
                 areaA=1,
                 areaAcapBk=1,
                 threshold=threshold)
}


for (i in 1:100) {
	cat(i, "...\n")
	examplestcd(i)
}



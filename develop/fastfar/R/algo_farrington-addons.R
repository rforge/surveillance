######################################################################
# New imlementation of algo.farrington.fitGLM which directly uses
# the glm.fit routine. This saves lots of overhead and increases
# speed.
#
# Author: Mikko Virtanen (@thl.fi) and Michael Hoehle
# Date:   9 June 2010
######################################################################

##  anscombe.residuals.local<-function(model,phi) {  
##    model$na.action<-na.omit
##    yy <- model$y
##    mu <- fitted.values(model)
##    hv<-hatvalues.lm(model)
##    ans <- 3/2 * (yy^(2/3) * mu^(-1/6) - mu^(1/2))
##    ans/sqrt(phi * (1 - hv))
##  }


######################################################################
# algo.farrington.fitGLM in a version using glm.fit which is faster
# than the call using "glm" in the ordinary version of algo.farrington.
# Note: Not all glm results may work on the output. But for the
# necessary ones for the algo.farrington procedure work.
######################################################################

algo.farrington.fitGLM.fast <- function(response,wtime,timeTrend=TRUE,reweight=TRUE) {
  #Create design matrix and formula needed for the terms object
  #Results depends on whether to include a time trend or not.
  if (timeTrend) {
    design<-cbind(intercept=1,wtime) 
    Formula<-response~wtime 
  } else {
    design<-matrix(1,nrow=length(wtime))
    Formula<-response~1
  }
  
  #Fit it using glm.fit which is faster than calling "glm"
  model <- glm.fit(design,response, family = quasipoisson(link = "log"))
      
   #Check convergence - if no convergence we return empty handed.
   if (!model$converged) {
      #Try without time dependence
     if (timeTrend) {
       model <- glm.fit(design[,1,drop=FALSE],response, family = quasipoisson(link = "log"))
       Formula<-response~1
       cat("Warning: No convergence with timeTrend -- trying without.\n")
     } 
   }

   #Fix class of output to glm/lm object in order for anscombe.residuals to work
   #Note though: not all glm methods may work for the result
   class(model) <- c("glm","lm")

   #Overdispersion parameter phi
   phi <- max(summary.glm(model)$dispersion,1)
    
   #In case reweighting using Anscome residuals is requested
   if (reweight) {
     s <- anscombe.residuals(model,phi)
     omega <- algo.farrington.assign.weights(s)
     model <- glm.fit(design,response, family = quasipoisson(link = "log"), weights = omega)
     #Here, the overdispersion often becomes small, so we use the max
     #to ensure we don't operate with quantities less than 1.
     phi <- max(summary.glm(model)$dispersion,1)
   } # end of refit.
    
   model$phi <- phi
   model$wtime <- wtime
   model$response <- response
   model$terms<-terms(Formula)
   class(model)<-c("algo.farrington.glm","glm") # cheating a bit, all methods for glm may not work
    #Done
    return(model)
 }



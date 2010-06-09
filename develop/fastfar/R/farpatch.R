*** surveillance/R/algo_farrington.R	2009-03-02 23:26:28.000000000 +0200
--- algo_farrington.R	2009-06-11 15:19:49.000000000 +0300
*************** anscombe.residuals <- function(m,phi) {
*** 11,16 ****
--- 11,24 ----
    a <- a/sqrt(phi * (1-hatvalues(m)))
    return(a)
  }
+ anscombe.residuals.local<-function(model,phi) {  
+   model$na.action<-na.omit
+   yy <- model$y
+   mu <- fitted.values(model)
+   hv<-hatvalues.lm(model)
+   ans <- 3/2 * (yy^(2/3) * mu^(-1/6) - mu^(1/2))
+   ans/sqrt(phi * (1 - hv))
+ }
  
  
  ###################################################
*************** algo.farrington.assign.weights <- functi
*** 31,46 ****
  ###################################################
  algo.farrington.fitGLM <- function(response,wtime,timeTrend=TRUE,reweight=TRUE) {
    #Model formula depends on whether to include a time trend or not.
!   theModel <- as.formula(ifelse(timeTrend, "response~1+wtime","response~1"))
  
    #Fit it.
!   model <- glm(theModel, family = quasipoisson(link="log"))
      
   #Check convergence - if no convergence we return empty handed.
    if (!model$converged) {
      #Try without time dependence
      if (timeTrend) {
!      model <- glm(response ~ 1, family = quasipoisson(link="log"))
       cat("Warning: No convergence with timeTrend -- trying without.\n")
      } 
  
--- 39,60 ----
  ###################################################
  algo.farrington.fitGLM <- function(response,wtime,timeTrend=TRUE,reweight=TRUE) {
    #Model formula depends on whether to include a time trend or not.
!   design<-cbind(intercept=1,wtime) # Design matrix
!   Formula<-response~wtime # needed for the terms object
!   if(!timeTrend) {
!     design<-design[,1,drop=FALSE]
!     Formula<-response~1
!   }
  
    #Fit it.
!   model <- glm.fit(design,response, family = quasipoisson(link = "log"))
      
   #Check convergence - if no convergence we return empty handed.
    if (!model$converged) {
      #Try without time dependence
      if (timeTrend) {
!       model <- glm.fit(design[,1,drop=FALSE],response, family = quasipoisson(link = "log"))
!       Formula<-response~1
       cat("Warning: No convergence with timeTrend -- trying without.\n")
      } 
  
*************** algo.farrington.fitGLM <- function(respo
*** 52,67 ****
    }
  
    #Overdispersion parameter phi
!   phi <- max(summary(model)$dispersion,1)
    
    #In case reweighting using Anscome residuals is requested
    if (reweight) {
!     s <- anscombe.residuals(model,phi)
      omega <- algo.farrington.assign.weights(s)
!     model <- glm(theModel,family=quasipoisson(link="log"),weights=omega)
      #Here, the overdispersion often becomes small, so we use the max
      #to ensure we don't operate with quantities less than 1.
!     phi <- max(summary(model)$dispersion,1)
    } # end of refit.
    
  
--- 66,81 ----
    }
  
    #Overdispersion parameter phi
!   phi <- max(summary.glm(model)$dispersion,1)
    
    #In case reweighting using Anscome residuals is requested
    if (reweight) {
!     s <- anscombe.residuals.local(model,phi)
      omega <- algo.farrington.assign.weights(s)
!     model <- glm.fit(design,response, family = quasipoisson(link = "log"), weights = omega)
      #Here, the overdispersion often becomes small, so we use the max
      #to ensure we don't operate with quantities less than 1.
!     phi <- max(summary.glm(model)$dispersion,1)
    } # end of refit.
    
  
*************** algo.farrington.fitGLM <- function(respo
*** 69,77 ****
--- 83,101 ----
    model$phi <- phi
    model$wtime <- wtime
    model$response <- response
+   model$terms<-terms(Formula)
+   class(model)<-c("algo.farrington.glm","glm") # cheating a bit, all methods for glm may not work
    #Done
    return(model)
  }
+ ### local simplified version of predict. may be redundant.
+ predict.algo.farrington.glm<-function(a,newdata,type="response",dispersion=1,se.fit=FALSE) {
+   pp<-predict.lm(a,newdata,se.fit,dispersion=as.vector(sqrt(dispersion)))
+   if(se.fit)
+     list(fit=a$family$linkinv(pp$fit),se.fit=pp$se.fit*abs(a$family$mu.eta(pp$fit)))
+   else
+     a$family$linkinv(pp)
+ }
  
  
  ###################################################
*************** algo.farrington <- function(disProgObj, 
*** 188,194 ****
        p <- summary.glm(model)$coefficients["wtime",4]
        significant <- (p < 0.05)
        #prediction for time k
!       mu0Hat <- predict.glm(model,data.frame(wtime=c(k)),type="response")
        #have to use at least three years of data to allow for a trend
        atLeastThreeYears <- (control$b>=3)
        #no horrible predictions
--- 212,218 ----
        p <- summary.glm(model)$coefficients["wtime",4]
        significant <- (p < 0.05)
        #prediction for time k
!       mu0Hat <- predict(model,data.frame(wtime=c(k)),type="response")
        #have to use at least three years of data to allow for a trend
        atLeastThreeYears <- (control$b>=3)
        #no horrible predictions
*************** algo.farrington <- function(disProgObj, 
*** 211,217 ****
      ######################################################################
      #Predict value - note that the se is the mean CI
      #and not the prediction error of a single observation
!     pred <- predict.glm(model,data.frame(wtime=c(k)),dispersion=model$phi,
                          type="response",se.fit=TRUE)
      #Calculate lower and upper threshold
      lu <- algo.farrington.threshold(pred,model$phi,skewness.transform=control$powertrans,alpha=control$alpha)
--- 235,241 ----
      ######################################################################
      #Predict value - note that the se is the mean CI
      #and not the prediction error of a single observation
!     pred <- predict(model,data.frame(wtime=c(k)),dispersion=model$phi,
                          type="response",se.fit=TRUE)
      #Calculate lower and upper threshold
      lu <- algo.farrington.threshold(pred,model$phi,skewness.transform=control$powertrans,alpha=control$alpha)

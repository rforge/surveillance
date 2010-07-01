algo.hhhcov <- function(disProgObj, covlambda=list(), covnu=list(), 
    control=list(nseason=0, initialvalues=NULL, interceptnu="one", 
    offset=FALSE, link="log", lambda=TRUE, names=TRUE)){
    
  T <- dim(disProgObj$observed)[1]                            # point in time
  I <- dim(disProgObj$observed)[2]                            # number of regions
  if(class(control$nseason)=="NULL")(control$nseason=0)    # seasonality (default values)
  S <- control$nseason                                                        
  J <- length(covlambda)                                      # number of covariables without intercept of lambda
  K <- length(covnu)                                          # number of covariables of nu
  
  #########################################################################
  # default values 
  if(class(control$interceptnu)=="NULL")(control$interceptnu="one")   
  if(class(control$offset)=="NULL")(control$offset=FALSE)                       
  if(class(control$link)=="NULL")(control$link="log")                        
  if(class(control$lambda)=="NULL")(control$lambda=TRUE)
  if(class(control$names)=="NULL")(control$names=TRUE)
  ######################################################################## 
  
  #########################################################################
  # add matrix to covlambda list to include the intercept if lambda=TRUE
  if(control$lambda==TRUE){
  tmp <- vector("list",J+1)
  if(!is.null(names(covlambda))) (names(tmp)=c("intercept",names(covlambda)))
  tmp[[1]] <- matrix(rep(1,T*I),nrow=T,ncol=I)
  j<-1
  while(j<=J){
    tmp[[1+j]]=covlambda[[j]]
    j=j+1
    }
  covlambda <- tmp
  J <- length(covlambda)
  }
  #########################################################################
  # short control for offset whether all fractions are the same or not
  if(sum(abs(disProgObj$populationFrac-matrix(1/I,nrow=T,ncol=I)))==0 & control$offset==TRUE){
    print("Remark: all regions have the same offset")
  }
  
  #########################################################################
  # default values for initial values 
  if(class(control$initialvalues)=="NULL"){
    control$initialvalues <- startvalues.cov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control)
  }
  ########################################################################   
  
  
  ########################################################################   
  error <- control.modelcov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control)
  if(class(error)!="NULL"){
    cat("error message:",error,"\n")
    return()
  }
  ########################################################################                     
  
  ########################################################################   
  control_model <- control
  start <- control_model$initialvalues
  L <- optim(start,log.cov,score.cov,disProgObj,covlambda,covnu,control_model,method="BFGS",control=c(fnscale=-1),hessian=TRUE)
  ########################################################################   
  
  
  ########################################################################   
  #convergence$
  c <- L$convergence
  d <- det(L$hessian)
  inv <- try(solve(-L$hessian))
  if(c==0&d!=0){convergence=0}  #"successful convergence"
  if(c==1&d!=0){convergence=1}  #"iteration limit maxit had been reached, but Hessian matrix is invertible"
  if(c==0&d==0){convergence=2}  #"optim() says successful convergence, but Hessian matrix is not invertible"
  if(c==0&inherits(inv,'try-error')){convergence=2}  #"optim() says successful convergence, but Hessian matrix is not invertible"
  if(c!=0&c!=1){convergence=3}  #"no convergence"
  ########################################################################   
  
  ########################################################################   
  # standard error
  if(!inherits(inv,'try-error')){
    cov <- inv
    se <- ((diag(cov))^(1/2))
  } else{
    cov <- NULL
    se <- NULL
  }
  ########################################################################  
  
  ########################################################################   
  # Mu, Lambda, Nu and AIC
  m <- model.cov(values=L$par,disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model)
  if(control$interceptnu=="one"){
    r=1
  } else {
    r=I
  }
  AIC <- 2*(-L$value+(J+r+2*S+K))
  ########################################################################                    
  
  ########################################################################   
  # names of coefficients
  # beta
  if(!is.null(m$beta)){
    if(sum(names(covlambda)=="")==0 & !is.null(names(covlambda)) & control$names==TRUE){
      names <- substring(paste("lambda",names(covlambda),sep="_"),first=1,last=20)
      names(m$beta) <- names[1:m$J]
    } else{
      names <- substring(c("lambda_intercept",paste("lambda_beta",2:m$J,sep="_")),first=1,last=20)
      names(m$beta) <- names[1:m$J]
    }
  }
  
  # alpha
  if(!is.null(m$alpha)){
    if(control$names==TRUE){
      names=substring(paste("nu",disProgObj$state[,1],sep="_"),first=1,last=20)
      if(r==1){
        names(m$alpha)=c("nu_intercept")
      } else {
        names(m$alpha)=names
      }
    } else{
      names <- substring(paste("nu_region",1:r,sep="_"),first=1,last=20)
      if(r==1) {
        names(m$alpha)=c("nu_intercept")
      } else {
        names(m$alpha)=names
      }
    }
  }
  
  # gamma
  if(!is.null(m$gamma)){
    period <- disProgObj$freq
    s <- 1
    mod <- paste("sin(", 2 * s, "*pi*t/", period,")",sep="")
    s <- 2
    while(s<=S){
      mod <- c(mod,paste("sin(", 2 * s, "*pi*t/", period,")",sep=""))
      s <- s+1
    }
    names <- substring(paste("gamma",mod,sep=" "),first=1,last=20)
    names(m$gamma) <- names
  }
  
  # delta
  if(!is.null(m$delta)){
    period <- disProgObj$freq
    s <- 1
    mod <- paste("cos(", 2 * s, "*pi*t/", period,")",sep="")
    s <- 2
    while(s<=S){
      mod  <- c(mod,paste("cos(", 2 * s, "*pi*t/", period,")",sep=""))
      s <- s+1
    }
    names <- substring(paste("delta",mod,sep=" "),first=1,last=20)
    names(m$delta) <- names
  }
  
  # beta.tilde
  if(!is.null(m$beta.tilde)){
    if(sum(names(covnu)=="")==0 & !is.null(names(covnu)) & control$names==TRUE){
      names <- substring(paste("nu",names(covnu),sep="_"),first=1,last=20)
      names(m$beta.tilde) <- names[1:m$K]
    } else{
      names <- substring(paste("nu_beta",1:m$K,sep="_"),first=1,last=20)
      names(m$beta.tilde) <- names[1:m$K]
    }
  }
  
  # coefficients
  coefficients <- c(m$beta,m$alpha,m$gamma,m$delta,m$beta.tilde)
  # se
  if(!is.null(se)){
    names(se) <- names(coefficients)
  }
  ########################################################################                    
  
  ########################################################################   
  # log likelihood per region
  loglikelihood <- log.cov(values=coefficients,disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control)
  ########################################################################   
  
  
  result <- list(coefficients=coefficients, se=se, cov=cov, call=match.call(), 
    loglikelihood=loglikelihood, AIC=AIC, convergence=convergence, 
    fitted=m$Mu, Lambda=m$Lambda, Nu=m$Nu, 
    disProgObj=disProgObj, covlambda=covlambda, covnu=covnu, control=control_model)
  
  class(result) <- "ahcov"
  return(result)
}

#---------------------------------------------------------
algo.hhhcov.grid <- function(disProgObj, covlambda=list(), covnu=list(),
    control=list(nseason=0, interceptnu="one", offset=FALSE, link="log", 
      lambda=TRUE,names=TRUE), startMatrix, maxTime=180, verbose=FALSE){

  T <- dim(disProgObj$observed)[1]                            # point in time
  I <- dim(disProgObj$observed)[2]                            # number of regions
  if(class(control$nseason)=="NULL")(control$nseason=0)    # seasonality (default values)
  S <- control$nseason                                                        
  J <- length(covlambda)  # number of covariables without intercept of lambda
  K <- length(covnu)      # number of covariables of nu
  
  #########################################################################
  # default values 
  if(class(control$interceptnu)=="NULL")(control$interceptnu="one")   
  if(class(control$offset)=="NULL")(control$offset=FALSE)                       
  if(class(control$link)=="NULL")(control$link="log")                        
  if(class(control$lambda)=="NULL")(control$lambda=TRUE)
  if(class(control$names)=="NULL")(control$names=TRUE)
  if(!is.logical(verbose))(return("verbose must be 'TRUE' or 'FALSE'"))
  ######################################################################## 
  
  ########################################################################   
  error <- control.modelcovgrid(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control)
  if(class(error)!="NULL"){
    cat("error message:",error,"\n")
    return(NULL)
  }
  ###################
  
  ########################################################################
  # control dimension of startMatrix has initialvalues the right number of parameters
  if(control$interceptnu=="one"){
    r <- 1
  } else{
    r <- I
  }
  if(control$lambda==TRUE) (J=J+1)
  if(!is.matrix(startMatrix)){
    cat("startMatrix must be a matrix with",J+r+2*S+K,"columns and the start values in the row\n")
    return(NULL) 
  }
  if(ncol(startMatrix)!=(J+r+2*S+K)){
    cat("startMatrix must be a matrix with",J+r+2*S+K,"columns\n")
    return(NULL)
  }
  ########################################################################
  
  niter <- nrow(startMatrix)                                    # number of start values
  gridused <- niter
  time <- maxTime
  bestLoglik <- list(loglikelihood=-1e+99)                      # list with the best result
  allLoglik <- matrix(NA,nrow=niter,ncol=(1+ncol(startMatrix))) # matrix with overview of coefficients and log(L)
  
  
  i=0
  while((time>0) & (i<niter)){
      i <- i+1
      control$initialvalues <- startMatrix[i,]
      time.i <- system.time(res<-try(algo.hhhcov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control=control),silent=!verbose))[3]
      time <- time-time.i
      if(verbose){
        if(inherits(res,'try-error')){ 
          print(c(niter=i,timeLeft=time,loglik=NULL)) 
        } else {
          print(c(niter=i,timeLeft=time,loglik=res$loglikelihood))  
        }
      }
      if(!inherits(res,'try-error') && res$convergence==0){
        allLoglik[i,] <- c(res$loglikelihood,res$coefficients)
        if(res$loglikelihood>bestLoglik$loglikelihood) bestLoglik <- res
      }
  }
  
  if(time<0){
      if(verbose) cat("Time limit exceeded, grid search stopped after",i,"iterations. \n")
      allLoglik <- as.matrix(allLoglik[1:i,])
      gridused <- i
  }
  
  timeused <- ifelse(time>0,maxTime-time,maxTime+abs(time))
  
  if(is.null(bestLoglik$coefficients)){
      bestLoglik <- list(loglikelihood=NULL,convergence=3)
  } else {
      colnames(allLoglik) <- c("loglik",names(bestLoglik$coefficients))
  }
  
  
  result <- list(best=bestLoglik,allLoglik=allLoglik,gridSize=niter,gridUsed=gridused,time=timeused,convergence=bestLoglik$convergence)
  class(result) <- "ahcovg"
  return(result)
}

#-----------------------------------------------------------
model.cov <- function(values, disProgObj, covlambda, covnu, control_model){
  control <- control_model
  s <- separate.cov(values=values,disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model)
  T <- s$T
  J <- s$J
  I <- s$I
  S <- s$S
  K <- s$K
  beta <- s$beta
  alpha <- s$alpha
  gamma <- s$gamma
  delta <- s$delta
  beta.tilde <- s$beta.tilde
  
  ########################################################################
  # produce Lambda matrix
  if(control$lambda==TRUE & J>0){
    A <- matrix(0,nrow=T,ncol=I)
    j <- 1
    while(j<=J){
      A <- A+covlambda[[j]]*beta[j]
      j <- j+1
    }
    if(control$link=="log") (Lambda=exp(A))
    if(control$link=="logit") (Lambda=1/(1+exp(-A)))       
  }                    
  if(control$lambda!=TRUE & J==0) (Lambda=NULL) 
  
  ########################################################################
  # produce Nu matrix
  
  #alpha
  if(control$interceptnu=="one"){
    a <- rep(alpha,I)
  } else {
    a <- alpha
  }
  A <- matrix(rep(a,T),nrow=T,byrow=TRUE)
  # gamma and delta
  if(S>0){
    mod <- "~-1"
    period <- disProgObj$freq
    for(s in 1:S){
      mod <- paste(mod, "+sin(", 2 * s, "*pi*t/", period, 
                        ")+cos(", 2 * s, "*pi*t/", period, ")", sep = "")
    }
    mod <- as.formula(mod)
    M <- model.matrix(mod, data.frame(t = 1:T))
    Sinus <- as.matrix(M[,seq(from=1,to=2*S,2)])
    Cosinus <- as.matrix(M[,seq(from=2,to=2*S,2)])
    a <- Sinus%*%gamma+Cosinus%*%delta   
    A <- A+matrix(rep(a,I),ncol=I)
  }
  # beta.tilde
  if(K>0){
    k <- 1
    while(k<=K){
      A <- A+covnu[[k]]*beta.tilde[k]
      k <- k+1
    }
  }
  Nu <- exp(A)
  
  #######################################################################
  # produce Mu matrix
  # offset with fraction of population
  if(control$offset==TRUE){
    N <- disProgObj$populationFrac
  } else{
    N <- matrix(1,nrow=T,ncol=I)
  }
  Mu <- matrix(NA,nrow=T,ncol=I)  # first row will keep NA because mu is defined for t>=2
  if(control$lambda==TRUE){
    Mu[-1,] <- as.matrix(Lambda[-1,]*disProgObj$observed[-T,]+N[-1,]*Nu[-1,])
  } else{
    Mu[-1,] <- as.matrix(N[-1,]*Nu[-1,])
  }
  ########################################################################
  return(list(Mu=Mu, Lambda=Lambda, Nu=Nu, N=N,T=T,I=I,S=S,J=J,K=K, 
    beta=beta,alpha=alpha,gamma=gamma,delta=delta,beta.tilde=beta.tilde))
}

#-------------------------------------------------------------
log.cov <- function(values, disProgObj, covlambda, covnu, control_model){
  m <- model.cov(values=values,disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model)
  i <- 1
  v <- rep(0,m$I)
  while(i<=m$I){#calculating log-likelihood for each region
    v[i] <- sum(dpois(disProgObj$observed[-1,i],lambda=m$Mu[-1,i],log=TRUE))
    i <- i+1
  }
  V <- sum(v)
  attr(V,"unit") <- v
  return(V)              #returns the value of log-likelihood
}

#-------------------------------------------------------------
score.cov <- function(values, disProgObj, covlambda, covnu, control_model){
  control <- control_model
  m <- model.cov(values=values,disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model)
  T <- m$T
  J <- m$J
  I <- m$I
  S <- m$S
  K <- m$K
  Mu <- m$Mu
  Lambda <- m$Lambda
  Nu <- m$Nu
  N <- m$N
  
  Z <- disProgObj$observed
  A <- as.matrix(Z[-1,]/Mu[-1,]-1)
  freq <- disProgObj$freq
  D <- diff.cov(Lambda=Lambda,Nu=Nu,covlambda=covlambda,covnu=covnu,T=T,J=J,I=I,S=S,K=K,freq=freq,control_model=control_model)
  
  if(control$interceptnu=="one"){
    r <- 1
  } else{
    r <- I
  }                
  Score <- rep(NA,(J+r+S+S+K))
  # Score(beta)
  if(J>0){
    for(j in 1:J){
      Score[j] <- sum(A*Z[-T,]*D$dbeta[[j]][-1,])
    }
  }
  
  # Score(alpha)
  salpha <- rep(NA,I)
  for(i in 1:I){
    salpha[i] <- colSums(A*N[-1,]*D$dalpha[[i]][-1,])[i]
  }
  if(r==1){
    Score[J+r] <- sum(salpha)
  } else{
    Score[(J+1):(J+r)] <- salpha
  }
  
  # Score(gamma)
  if(S>0){
    for(s in 1:S){
      Score[(J+r+s)] <- sum(A*N[-1,]*D$dgamma[[s]][-1,])
    }
  }
  
  # Score(delta)
  if(S>0){
    for(s in 1:S){
      Score[(J+r+S+s)] <- sum(A*N[-1,]*D$ddelta[[s]][-1,])
    }
  }
  
  # Score(beta.tilde)
  if(K>0){
    for(k in 1:K){
      Score[(J+r+2*S+k)] <- sum(A*N[-1,]*D$dbeta.tilde[[k]][-1,])
    }
  }
  
  return(Score)              #returns the score vector
}

#-------------------------------------------------------------
diff.cov=function(Lambda,Nu,covlambda,covnu,T,J,I,S,K,freq,control_model){
  control <- control_model
  if(control$link=="log"){
    dbeta <- vector("list",J)
    j <- 1
    while(j<=J){
      dbeta[[j]] <- covlambda[[j]]*Lambda
      j <- j+1
    }
    
    dalpha <- vector("list",I)
    i <- 1
    while(i<=I){
      dalpha[[i]] <- matrix(0,nrow=T,ncol=I)
      dalpha[[i]][,i] <- Nu[,i]
      i <- i+1
    }
    
    dgamma <- vector("list",S)
    s <- 1
    while(s<=S){
      dgamma[[s]] <- sin(2*s*pi*c(1:T)/freq)*Nu
      s <- s+1
    }
    ddelta <- vector("list",S)
    s <- 1
    while(s<=S){
      ddelta[[s]] <- cos(2*s*pi*c(1:T)/freq)*Nu
      s <- s+1
    } 
    
    dbeta.tilde <- vector("list",K)
    k <- 1
    while(k<=K){
      dbeta.tilde[[k]] <- covnu[[k]]*Nu
      k <- k+1
    }
  }
  
  if(control$link=="logit"){
    dbeta <- vector("list",J)
    j <- 1
    while(j<=J){
      dbeta[[j]] <- covlambda[[j]]*Lambda*(1-Lambda)
      j <- j+1
    }
    dalpha <- vector("list",I)
    i <- 1
    while(i<=I){
      dalpha[[i]] <- matrix(0,nrow=T,ncol=I)
      dalpha[[i]][,i] <- Nu[,i]
      i <- i+1
    }
    dgamma <- vector("list",S)
    s <- 1
    while(s<=S){
      dgamma[[s]] <- sin(2*s*pi*c(1:T)/freq)*Nu
      s <- s+1
    }
    ddelta <- vector("list",S)
    s <- 1
    while(s<=S){
      ddelta[[s]] <- cos(2*s*pi*c(1:T)/freq)*Nu
      s <- s+1
    } 
    dbeta.tilde <- vector("list",K)
    k <- 1
    while(k<=K){
      dbeta.tilde[[k]] <- covnu[[k]]*Nu
      k <- k+1
    }
  }
  return(list(dbeta=dbeta,dalpha=dalpha,dgamma=dgamma,ddelta=ddelta,dbeta.tilde=dbeta.tilde))
}




#-------------------------------------------------------------
control.modelcov <- function(disProgObj, covlambda, covnu, control_model){
  control <- control_model
  T <- dim(disProgObj$observed)[1] # point in time
  I <- dim(disProgObj$observed)[2] # number of regions
  S <- control$nseason             # seasonality                   
  J <- length(covlambda)           # number of covariables of lambda
  K <- length(covnu)               # number of covariables of nu
  
  # covlambda and covnu
  # dimension of covlambda
  if(J>0){
    W <- sapply(covlambda,class)
    if(sum(W=="matrix")!=J){
      return("arguments in covlambda must be matrices")
    } else{
      W <- sapply(covlambda,dim) 
      if((sum(W[1,]==rep(T,J))!=J)) (return("dimension of matrices in covlambda are not correct"))
      if((sum(W[2,]==rep(I,J))!=J)) (return("dimension of matrices in covlambda are not correct"))
    }
  }
  # dimension of covnu
  if(K>0){
    Y <- sapply(covnu,class)
    if(sum(Y=="matrix")!=K){
      return("arguments in covnu must be matrices")
    } else{
      Y <- sapply(covnu,dim)
      if((sum(Y[1,]==rep(T,K))!=K)) (return("dimension of matrices in covnu are not correct"))
      if((sum(Y[2,]==rep(I,K))!=K)) (return("dimension of matrices in covnu are not correct"))
    }
  }
  
  # nseason
  if(S<0) (return("nseason must be >=0"))
  
  # interceptnu 
  if(sum(control$interceptnu==c("one","region"))!=1) (return("interceptnu must be 'one' or 'region'"))
  
  # link
  if(sum(control$link==c("log","logit"))!=1) (return("link must be 'log' or 'logit'"))
  
  # lambda
  if(!is.logical(control$lambda)) (return("lambda must be 'TRUE' or 'FALSE'"))
  # control$lambda and covlambda: if lambda=TRUE and covlambda=list() there will be the intercept
  if(control$lambda==FALSE & J>0) (return("lambda=FALSE but covlambda has matrices"))
  
  # offset
  if(!is.logical(control$offset)) (return("offset must be 'TRUE' or 'FALSE'"))
  
  # has initialvalues the right number of parameters
  if(control$interceptnu=="one"){
    r <- 1
  } else {
    r <- I
  }
  if(length(control$initialvalues)!=(J+r+2*S+K)){
    return(paste("The initial value has not the right length with",
       length(control$initialvalues),"and",J+r+2*S+K,"unknown parameters"))
  }
}

#----------------------------------------------
control.modelcovgrid <- function(disProgObj, covlambda, covnu, control_model){
  control <- control_model
  T <- dim(disProgObj$observed)[1] # point in time
  I <- dim(disProgObj$observed)[2] # number of regions
  S <- control$nseason    # seasonality
  J <- length(covlambda)  # number of covariables of lambda
  K <- length(covnu)      # number of covariables of nu
  
  # covlambda and covnu
  # dimension of covlambda
  if(J>0){
    W <- sapply(covlambda,class)
    if(sum(W=="matrix")!=J){
      return("arguments in covlambda must be matrices")
    } else{
      W <- sapply(covlambda,dim)
      if((sum(W[1,]==rep(T,J))!=J)) (return("dimension of matrices in covlambda are not correct"))
      if((sum(W[2,]==rep(I,J))!=J)) (return("dimension of matrices in covlambda are not correct"))
    }
  }
  # dimension of covnu
  if(K>0){
    Y <- sapply(covnu,class)
    if(sum(Y=="matrix")!=K){
      return("arguments in covnu must be matrices")
    } else{
      Y <- sapply(covnu,dim)
      if((sum(Y[1,]==rep(T,K))!=K)) (return("dimension of matrices in covnu are not correct"))
      if((sum(Y[2,]==rep(I,K))!=K)) (return("dimension of matrices in covnu are not correct"))
    }
  }
  
  # nseason
  if(S<0) (return("nseason must be >=0"))
  
  # interceptnu
  if(sum(control$interceptnu==c("one","region"))!=1) (return("interceptnu must be 'one' or 'region'"))
  
  # link
  if(sum(control$link==c("log","logit"))!=1) (return("link must be 'log' or 'logit'"))
  
  # lambda
  if(!is.logical(control$lambda)) (return("lambda must be 'TRUE' or 'FALSE'"))
  # control$lambda and covlambda: if lambda=TRUE and covlambda=list() there will be the intercept
  if(control$lambda==FALSE & J>0) (return("lambda=FALSE but covlambda has matrices"))
  
  # offset
  if(!is.logical(control$offset)) (return("offset must be 'TRUE' or 'FALSE'"))
}

#-------------------------------------------------------
separate.cov <- function(values, disProgObj, covlambda, covnu, control_model){
  control <- control_model
  T <- dim(disProgObj$observed)[1] # point in time
  I <- dim(disProgObj$observed)[2] # number of regions
  S <- control$nseason    # seasonality                  $  
  J <- length(covlambda)  # number of covariables of lambda
  K <- length(covnu)      # number of covariables of nu
   
  if(control$interceptnu=="one") {
    alpha <- values[J+1]
    r <- 1
  } else {
    alpha <- values[(J+1):(J+I)]
    r <- I
  }
  beta <- gamma <- delta <- beta.tilde <- NULL
  if(control$lambda==TRUE & J>0) beta <- values[1:J]
  if(S>0) gamma <- values[(J+r+1):(J+r+S)]
  if(S>0) delta <- values[(J+r+S+1):(J+r+2*S)]
  if(K>0) beta.tilde <- values[(J+r+2*S+1):(J+r+2*S+K)]
  
  return(list(T=T,I=I,S=S,J=J,K=K,beta=beta,alpha=alpha,gamma=gamma,delta=delta,beta.tilde=beta.tilde))
}

#--------------------------------------
startvalues.cov <- function(disProgObj,covlambda,covnu,control_model,lambda=0.6){

  control <- control_model
  S <- control$nseason             # seasonality                    
  J <- length(covlambda)           # number of covariables of lambda
  K <- length(covnu)               # number of covariables of nu
  T <- dim(disProgObj$observed)[1] # point in time
  I <- dim(disProgObj$observed)[2] # number of regions
  
  
  # offset with fraction of population
  if(control$offset==TRUE){
    N <- disProgObj$populationFrac
  } else {
    N <- matrix(1,nrow=T,ncol=I)
  }
  
  # fix lambda startvalue to 0.6 if lambda=TRUE
  if(control$lambda==TRUE){
    lambda <- lambda
  } else { 
    lambda <- 0
  }
  # mu=nu/(1-lambda) respectively =n*nu/(1-lambda), where n is population fraction
  nu <- (1-lambda)*colMeans(disProgObj$observed)/N[1,]
  
  # alpha, delta=gamma=beta.tile=0 if there exist
  startalpha <- nu
  startalpha[(nu>0)==TRUE] <- log(startalpha[(nu>0)==TRUE])
  if(control$interceptnu=="one") (startalpha <- mean(startalpha))
  
  if(control$lambda==TRUE){
    # if lambda=TRUE only intercept is taken and all other=0
    if(control$link=="log"){
      beta_1<-log(lambda)
    } else{
      beta_1<-log(lambda/(1-lambda))
    }
    
    start <- c(beta_1,rep(0,(J-1)),startalpha,rep(0,(2*S+K)))
  } else {
    start <- c(startalpha,rep(0,(2*S+K)))
  }
  
  return(start)
}

#--------------------------------------
startMatrix.cov <- function(disProgObj, covlambda, covnu, control_model,lambda=0.6,varycol=c(1),vary=c(0)){
  l <- length(lambda)
  vc <- length(varycol)
  v <- length(vary)
  
  x <- NULL
  for(i in 1:l){
    x <- rbind(x, startvalues.cov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model,lambda=lambda[i]))
  }
#   i <- 1
#   x <- startvalues.cov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model,lambda=lambda[i])
#   i <- 2
#   while(i<=l){
#   a <- startvalues.cov(disProgObj=disProgObj,covlambda=covlambda,covnu=covnu,control_model=control_model,lambda=lambda[i])
#   i <- i+1
#   x <- rbind(x,a)
#   }
  if(l==1)(x <- t(as.matrix(x)))
  
  if(vc==1){
    comb <- expand.grid(vary)
  } else{
    L <- vector("list",vc)
    for(i in 1:vc){
      L[[i]] <- vary
    }
    comb <- expand.grid.cov(L)
  }
  ncomb <- nrow(comb)
  
  Comb <- matrix(0,nrow=ncomb,ncol=ncol(x))
  for(i in 1:vc){
    k <- varycol[i]
    Comb[,k] <- comb[,i]
  }
  
  Matrix <- NULL
  for(i in 1:l){
    Matrix <- rbind(Matrix, Comb+matrix(rep(x[i,],ncomb),nrow=ncomb,byrow=TRUE) )
  }
  
#   i <- 1
#   Matrix <- Comb+matrix(rep(x[i,],ncomb),nrow=ncomb,byrow=TRUE)
#   i <- 2
#   while(i<=l){
#     a <- Comb+matrix(rep(x[i,],ncomb),nrow=ncomb,byrow=TRUE)
#     Matrix <- rbind(Matrix,a)
#     i <- i+1
#   }

  return(Matrix)
}

#--------------------------------------
expand.grid.cov <- function(..., KEEP.OUT.ATTRS = TRUE)
{
    nargs <- length(args <- list(...))
    a <- vector("list",length(args[[1]]))
    for(i in 1:length(args[[1]])){
      a[[i]] <- args[[1]][[i]]
    }
    args <- a
    nargs <- length(args)
    if (!nargs)
        return(as.data.frame(list()))
    if (nargs == 1L && is.list(a1 <- args[[1L]]))
        nargs <- length(args <- a1)
    if (nargs == 0L)
        return(as.data.frame(list()))
    cargs <- args
    nmc <- paste("Var", 1L:nargs, sep = "")
    nm <- names(args)
    if (is.null(nm))
        nm <- nmc
    else if (any(ng0 <- nzchar(nm)))
        nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1
    d <- sapply(args, length)
    if (KEEP.OUT.ATTRS) {
        dn <- vector("list", nargs)
        names(dn) <- nmc
    }
    orep <- prod(d)
    if (orep == 0L) {
        for (i in seq_len(nargs)) cargs[[i]] <- args[[i]][FALSE]
    }
    else {
        for (i in seq_len(nargs)) {
            x <- args[[i]]
            if (KEEP.OUT.ATTRS)
                dn[[i]] <- paste(nmc[i], "=", if (is.numeric(x))
                  format(x)
                else x, sep = "")
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,
                nx)), orep)]
            if (!is.factor(x) && is.character(x))
                x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if (KEEP.OUT.ATTRS)
        attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
    rn <- .set_row_names(as.integer(prod(d)))
    structure(cargs, class = "data.frame", row.names = rn)
}

#----------------------------------------- amplitudeShift = TRUE, reparamPsi = TRUE,
print.ahcov <- function (x, digits = max(3, getOption("digits") - 3),  ...){
  if (x$convergence>0)
    cat("Results are not reliable! Try different starting values. \n","convergence= ",x$convergence,"\n")
  else {
    if (!is.null(x$call)) {
      cat("Call: \n")
      print(x$call)
    }
    cat("\nEstimated parameters and standard errors: \n\n")
    
    Estimates <- as.matrix(x$coef)
    colnames(Estimates)  <- c("Estimates")
    Std.Error <- as.matrix(x$se)
    colnames(Std.Error)  <- c("Std.Error")
    print(round(cbind(Estimates, Std.Error),digits=digits), print.gap = 2)

    cat("\nlog-likelihood:   ", round(x$loglikelihood, digits = digits -2))
    cat("\nAIC:              ", round(x$AIC, digits = digits - 2), "\n")
    
    cat("\nnumber of units:   ", round(dim(x$disProgObj$observed)[2], digits = digits - 2))
    cat("\npoint in time:     ", round(dim(x$disProgObj$observed)[1], digits = digits - 2), "\n")    
  }
}

#----------------------------------------------,amplitudeShift=TRUE,reparamPsi = TRUE
print.ahcovg <- function (x, digits = max(3, getOption("digits") - 3), ...){
  cat("\nsize of grid: ", x$gridSize, "\n")
  if (x$gridSize != x$gridUsed) 
    cat("grid search stopped after", x$gridUsed, "iterations \n")
  cat("convergences: ", sum(!is.na(x$all[, 1])), "\n")
  cat("time needed (in seconds)", x$time, "\n\n")
  if (x$convergence!=0) 
    cat("\nAlgorithms did not converge, please try different starting values! \n")
  else {
    x$best$call <- NULL
    cat("values of log-likelihood:")
    print(table(round(x$all[, 1], 0)))
    print.ahcov(x$best, digits = digits) #, amplitudeShift = amplitudeShift, reparamPsi = reparamPsi)
  }
}

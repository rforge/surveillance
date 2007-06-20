library(surveillance)

outbrks <- c("m1", "m2", "m3", "m4", "m5", "q1_nrwh", "q2", 
              "s1", "s2", "s3", "k1", "n1", "n2", "h1_nrwrp")

#Convert all RKI data to 52 week objects
convert <- function(name) {
  e <- substitute(Objname <- readData(name,week53to52=TRUE),list(Objname=name))
  eval(e)
  save(list=name,file=paste("z:/Surveillance/surveillance/data/",name,".RData",sep=""))
}

sapply(outbrks,convert)

#Ordinary function but now with matrix args
correct53to52 <- function(disProgObj, firstweek = 1){

        if(firstweek > length(disProgObj$observed)){
                stop("firstweek doesn't exist")
        }

        observed <- disProgObj$observed
        state <- disProgObj$state
        week <- disProgObj$week
        
        if(dim(state)[1] != dim(observed)[1]){
                stop("state and observed don't have the same length")
        }

        # do not cut, if observed is too short
        length = dim(observed[firstweek:dim(observed)[1],])[1]

        if(length > 53){

                lastyear <- floor((length-1)/53)
                # sum case numbers of double weeks up
                for(i in 1:lastyear){
                        # last week of year i (-i+1 because the array now is shorter)
                        last <- firstweek + i * 52
                        # first week in year i+1
                        firstnew <- last + 1
                        observed[firstnew,]  <- observed[last,]  + observed[firstnew,]
                        # delete double weeks
                        observed <- observed[-c(last),]

                        # with state
                        state[firstnew,]  <- state[last,]  + state[firstnew,]
                        # delete double weeks
                        state <- state[-c(last),]
                        week <- week[-c(last)]
                }
        }

        
        # correct all 2 to 1
        state[state==2] <- 1

        disProgObj$observed <- observed
        disProgObj$state <- state
        disProgObj$week <- week
        return(disProgObj)
}


#Do hepa
file <- paste(Sys.getenv("HOME"),"Surveillance/surveillance/develop/hepAmale.txt",sep="")
hepMale <- as.matrix(read.table(file,header=TRUE,skip=1))
state <- matrix(0,dim(hepMale)[1],dim(hepMale)[2]-3)
state[291:294,] <- 1
hepa <- create.disProg(week=hepMale[,1],observed=hepMale[,-c(1,2,3)],state=state)
hepa <- correct53to52(hepa)

ha.berlin <- hepa
ha <- hepa#aggregate(hepa)
save(list=c("ha"),file=paste(Sys.getenv("HOME"),"Surveillance/package/surveillance/data/ha.RData",sep=""))
save(list=c("ha.berlin"),file=paste(Sys.getenv("HOME"),"/Surveillance/surveillance/data/hepa.berlin.RData",sep=""))

#
data(ha)
ha.berlin <- aggregate(ha)
save(list=c("ha.berlin"),file=paste(Sys.getenv("HOME"),"/Surveillance/surveillance/trunk/data/ha.berlin.RData",sep=""))

#load(file="z:/Surveillance/package/surveillance/data/hepa.RData")


##The salmonella hadar cases
x <- scan("shadar.txt",quiet=TRUE)
shadar <- create.disProg(1:length(x), x, rep(0,length(x)))

save(list=c("shadar"),file=paste(Sys.getenv("HOME"),"Surveillance/surveillance/data/shadar.RData",sep=""))


##########################
# Add freq argument to the disProg object

outbrks <- c("m1", "m2", "m3", "m4", "m5", "q1_nrwh", "q2", 
              "s1", "s2", "s3", "k1", "n1", "n2", "h1_nrwrp")

add.freq <- function(name) {
  #Put "name" into x
  e1 <- substitute(data(name),list(name=name))
  eval(e1)
  e2 <- gsub("\"","",deparse(substitute(x <- name,list(name=name))))
  write(e2,file="/tmp/foobar")
  eval(parse(file="/tmp/foobar"))

  #Add freq attribute
  if (class(x) == "disProg") {
    #x$freq <- 52
    x$start <- c(2001,1)

    
    #Save as data
    eval(substitute(Objname <- x,list(Objname=name)))
    save(list=name,file=paste(Sys.getenv("HOME"),"/Surveillance/surveillance/trunk/data/",name,".RData",sep=""))
  }
  invisible()
}

#Additional data
outbrks <- c(outbrks,"ha","measels.weser","meningo.age","shadar")
#Convert
sapply(outbrks,add.freq)

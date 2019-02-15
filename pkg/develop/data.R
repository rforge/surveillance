# 'correct53to52' sums up and cuts a value from a splited last and first week of a year
#
# Parameter:
#       disProgObj - object of class disProgObj (including the observed and the state chain)
#       firstweek: the number of the first week in a year, default = 1
# ouput:
#       the new disProgObj (corrected to 52 weeks instead of 53 weeks a year)

correct53to52 <- function(disProgObj, firstweek = 1){

        if(firstweek > length(disProgObj$observed)){
                stop("firstweek doesn't exist")
        }

        observed <- disProgObj$observed
        state <- disProgObj$state

        if(length(state) != length(observed)){
                stop("state and observed don't have the same length")
        }

        # do not cut, if observed is too short
        length = length(observed[firstweek:length(observed)])

        if(length > 53){

                lastyear <- floor((length-1)/53)
                # sum case numbers of double weeks up
                for(i in 1:lastyear){
                        # last week of year i (-i+1 because the array now is shorter)
                        last <- firstweek + i * 52
                        # first week in year i+1
                        firstnew <- last + 1
                        observed[firstnew]  <- observed[last]  + observed[firstnew]
                        # delete double weeks
                        observed <- observed[-c(last)]

                        # with state
                        state[firstnew]  <- state[last]  + state[firstnew]
                        # delete double weeks
                        state <- state[-c(last)]
                }
        }

        # correct also the first week, if it doesn't is the beginning
        if(firstweek > 1){
                observed[firstweek] <- observed[firstweek] + observed[firstweek-1]
                observed <- observed[-c(firstweek-1)]
                state[firstweek] <- state[firstweek] + state[firstweek-1]
                state <- state[-c(firstweek-1)]
        }

        # correct all 2 to 1
        state[state==2] <- 1

        disProgObj$observed <- observed
        disProgObj$state <- state

        return(disProgObj)
}

# 'readData' reads the data of a specified disease of several years
#            and generates a state chain using the bulletin knowledge
#
# Parameter:
#   abb : abbreviation of the disease
#   week53to52: Boolean indicating whether to convert RKI 53 Weeks System to 52 weeks a year
readData <- function(abb,week53to52=TRUE,sysPath=FALSE){
  #Read depending on which path is requested
  if (sysPath) {
    if (packageVersion("surveillance") > "1.16.2")
      stop("the package no longer contains these txt files")
    #Prepend the systempath/data to the filename
    #hoehle 2012-07-24 - this does not work when package is not
    #installed. Use extdata as recommended in the file package structure.
    file <- file.path(path.package('surveillance'),'extdata',paste(abb,".txt",sep=""))
  } else {
    file <- file.path("data", paste(abb,".txt",sep=""))
  }

  # read the data from four years and write it to a table
  #file <- paste( dataPath, abb , ".txt" , sep="" )
  fileTable <- read.table( file=file, header=TRUE )
  observed <- fileTable$observed
  state <- fileTable$state

  result = list(observed=observed, state=state)

  class(result) = "disProg" # for disease progress

  #Convert to 52 week system...
  if (week53to52) {
    result <- correct53to52(result)
  }

  result$freq <- 52
  result$start <- c(2001,1)

  return(result)
}


outbrks <- c("m1", "m2", "m3", "m4", "m5", "q1_nrwh", "q2",
              "s1", "s2", "s3", "k1", "n1", "n2", "h1_nrwrp")

#Convert all RKI data to 52 week objects
convert <- function(name) {
  e <- substitute(Objname <- readData(name,week53to52=TRUE),list(Objname=name))
  eval(e)
  save(list=name,file=paste("../data/",name,".RData",sep=""))
}

sapply(outbrks,convert)


# --

library("surveillance")

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
hepMale <- as.matrix(read.table("hepAmale.txt",header=TRUE,skip=1))
state <- matrix(0,dim(hepMale)[1],dim(hepMale)[2]-3)
state[291:294,] <- 1
hepa <- create.disProg(week=hepMale[,1],observed=hepMale[,-c(1,2,3)],state=state)
hepa <- correct53to52(hepa)

ha <- hepa
save(list=c("ha"),file="../data/ha.RData")

## data(ha, package = "surveillance")
## ha.berlin <- aggregate(ha)
## save(list=c("ha.berlin"),file="../data/ha.berlin.RData")


##The salmonella hadar cases
x <- scan("shadar.txt",quiet=TRUE)
shadar <- create.disProg(1:length(x), x, rep(0,length(x)))

save(list=c("shadar"),file="../data/shadar.RData")

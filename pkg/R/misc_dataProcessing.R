###################################################
### chunk number 3:
###################################################

# 'correct53to52' sums up and cuts a value from a splited last and first week of a year
#
# Parameter:
#       disProgObj - object of class disProgObj (including the observed and the state chain)
#       firstweek: the number in observed of the first week in a year, default = 1
# ouput:
#       disProgObj: the new disProgObj


correct53to52 <- function(disProgObj, firstweek = 1){
    .Deprecated(package = "surveillance")

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


###################################################
### chunk number 4:
###################################################

enlargeData <- function(disProgObj, range = 1:156, times = 1){
    .Deprecated(package = "surveillance")

        # enlarge observed
        disProgObj$observed <- c(rep(disProgObj$observed[range], times), disProgObj$observed)
        # enlarge state
        disProgObj$state <- c(rep(disProgObj$state[range], times), disProgObj$state)

        return(disProgObj)
}


enlargeData <- function(disProgObj, range = 1:156, times = 1){
    .Deprecated(package = "surveillance")

        # enlarge observed
        disProgObj$observed <- c(rep(disProgObj$observed[range], times), disProgObj$observed)
        # enlarge state
        disProgObj$state <- c(rep(disProgObj$state[range], times), disProgObj$state)

        return(disProgObj)
}

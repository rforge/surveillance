###################################################
### chunk number 1:
###################################################

test <- function(data = c("k1", "m5"), range = 157:339){
  .Deprecated(package = "surveillance")
  res <- list()
  for(i in data){
    disProgObj <- readData(i,week53to52=TRUE)
    disProgObj <- enlargeData(disProgObj)
    survResults <- algo.call(disProgObj,
                             control = list(
                               list(funcName = "rki1", range = range),
                               list(funcName = "rki2", range = range),
                               list(funcName = "rki3", range = range),
                               list(funcName = "bayes", range = range,alpha=0.05)))
    res[[i]] <- algo.compare(survResults)
    cat("\n\n\n", i, " Res:\n")
    print(compMatrix.writeTable(res[[i]]))
  }
  sum <- algo.summary(res)
  cat("\n\nSummary:\n")
  print(compMatrix.writeTable(sum))
}


###################################################
### chunk number 3:
###################################################

makePlot <- function(outputpath, data = "k1", method = "rki1", name, disease, range = 157:339){
  .Deprecated(package = "surveillance")
        disProgObj <- readData(data,week53to52=TRUE)
        disProgObj <- enlargeData(disProgObj)
        res <- algo.call(disProgObj, control = list(list(funcName = method, range = range)))
        pdf(paste(outputpath, data, "_", method, "_plot.pdf", sep=""), width = 10)
                plot(res[[1]],name,disease)
        dev.off()
}

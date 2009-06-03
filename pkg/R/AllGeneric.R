# generate generic functions needed

#Make summary a generic function
setGeneric("summary")

#Conversion of some other functions
if(!isGeneric("plot")) setGeneric("plot", useAsDefault=plot)
if(!isGeneric("aggregate")) setGeneric("aggregate", useAsDefault=aggregate)

######################################################################
#Access and replace functions
######################################################################
if(!isGeneric("epoch")) setGeneric("epoch", function(x, as.Date=x@epochAsDate) standardGeneric("epoch"))
setMethod("epoch", "sts", function(x, as.Date=x@epochAsDate) {
  if (!as.Date) {
    return(x@week)
  } else {
    return(as.Date(x@week, origin="1970-01-01"))
  }
})

setGeneric("epoch<-", function(x, value) standardGeneric("epoch<-"))
setReplaceMethod("epoch", "sts", function(x, value) {
 x@week <- value
 x
})

######################################################################
#Some access functions similar to matrix/dataframe (definition in sts.R)
######################################################################
if(!isGeneric("nrow")) setGeneric("nrow", useAsDefault=nrow)
if(!isGeneric("ncol")) setGeneric("ncol", useAsDefault=ncol)
if(!isGeneric("colnames")) setGeneric("colnames", useAsDefault=colnames)

#New methods 
if(!isGeneric("as.data.frame")) setGeneric("as.data.frame", useAsDefault=as.data.frame)



################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Functions and methods to make step() work for twinstim objects
### (restricted to one component at a time)
###
### Copyright (C) 2013 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


### To make step() work, we are dealing with modified twinstim objects:
### object$formula is replaced by the result of terms(object), which selects only
### one of the two components!
### We let this special class inherit from "twinstim" such that, e.g.,
### extractAIC.twinstim is used for its objects. However, this is tricky since
### the classes are actually incompatible in the formula specification. Only
### methods which don't use the $formula part work, but this constraint holds
### for what is needed to run step().

twinstim_stependemic <- function (.fit)
{
    class(.fit) <- c("twinstim_stependemic", "twinstim")
    .fit$formula <- .fit$formula$endemic
    .fit
}
twinstim_stepepidemic <- function (.fit)
{
    class(.fit) <- c("twinstim_stepepidemic", "twinstim")
    .fit$formula <- .fit$formula$epidemic
    .fit
}


## In the first step() loop, object$call$formula is set to terms(object). Since
## there is no "formula" argument to twinstim(), we must remove it from the call
## before update()ing
.drop.formula.from.call <- function (object)
{
    object$call$formula <- NULL
    object
}


### special update- and terms-methods for use through stepComponent() below

update.twinstim_stependemic <- function (object, endemic, ..., evaluate = TRUE)
{
    cl <- match.call(expand.dots=TRUE)
    cl[[1L]] <- as.name("update.twinstim")
    cl$object <- as.call(list(quote(surveillance:::.drop.formula.from.call),
                              cl$object))
    res <- eval.parent(cl)
    
    ## we need to keep the special class such that step() will keep invoking
    ## the special update- and terms-methods on the result
    stepclass <- sub("update.", "", .Method, fixed=TRUE)
    ##<- or: .Class[1L], or: grep("step", class(object), value=TRUE)
    if (evaluate) {
        do.call(stepclass, alist(res))
    } else {
        substitute(surveillance:::FUN(res),
                   list(FUN=as.name(stepclass), res=res))
    }
}

update.twinstim_stepepidemic <- function (object, epidemic, ..., evaluate = TRUE)
{}
body(update.twinstim_stepepidemic) <- body(update.twinstim_stependemic)


terms.twinstim_stependemic <- terms.twinstim_stepepidemic <-
    function (x, ...) terms(x$formula)



### Function to perform AIC-based model selection (component-specific)
### This is essentially a wrapper around stats::step()

stepComponent <- function (object, component = c("endemic", "epidemic"),
                           scope = list(upper=object$formula[[component]]),
                           direction = "both", trace = 2, verbose = FALSE, ...)
{
    component <- match.arg(component)

    ## Convert to special twinstim class where $formula is the component formula
    object_step <- do.call(paste0("twinstim_step", component), alist(object))
    
    ## silent optimizations
    if (trace <= 2) object_step$call$optim.args$control$trace <- 0
    object_step$call$verbose <- verbose

    ## Run the selection procedure
    res <- step(object_step, scope = scope, direction = direction,
                trace = trace, ...)
    
    ## Restore original trace and verbose arguments
    if (trace <= 2) res$call$optim.args$control$trace <-
        object$call$optim.args$control$trace
    res$call$verbose <- object$call$verbose

    ## Convert back to original class
    newformula <- formula(res$formula)
    res$formula <- object$formula
    res$formula[[component]] <- newformula
    class(res) <- class(object)

    ## Done
    res
}


## add1.default and drop1.default would work through step() -- since
## object$formula is internally replaced by the requested component's formula --
## but not in general
## For completeness, we thus also define twinstim methods for add1 and drop1
## add1.twinstim <- function(object, scope,
##                           component = c("endemic", "epidemic"),
##                           k = 2, trace = 2, ...)
## {
##     component <- match.arg(component)
##     class(object) <- c(paste0("twinstim_step", component), "twinstim")
##     object$formula <- object$formula[[component]]
##     NextMethod(component = NULL)  # -> .default-method (the "component" argument will be part
##                   # of "..." and passed to extractAIC where it is unused)
## }
## drop1.twinstim <- add1.twinstim

################################################################################
### Part of the surveillance package, http://surveillance.r-forge.r-project.org
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Conveniently add sine-cosine terms to a model formula
###
### Copyright (C) 2010 Michaela Paul, 2013-2015 Sebastian Meyer
### $Revision$
### $Date$
################################################################################

## basically, 'sin(2*pi * t/period) + cos(2*pi * t/period)' is added to 'f'
addSeason2formula <- function (
    f = ~1,       # formula to enhance
    S = 1,        # number of sine/cosine pairs
    period = 52,  # periodicity of the sinusoidal wave
    timevar = "t" # name of the time variable
){
    ## return unchanged formula if S = 0
    if (max(S) == 0)
        return(f)
  
    f <- paste(deparse(f), collapse = "")
    ## create formula
    if (length(S) == 1 && S > 0) {
        for (i in seq_len(S)){
            f <- paste0(f,
                        " + sin(",2*i,"*pi*",timevar,"/",period,")",
                        " + cos(",2*i,"*pi*",timevar,"/",period,")")
        }
    } else {
        for (i in seq_len(max(S))){
            which <- paste(i <= S,collapse=",")
            f <- paste0(f,
                        " + fe( sin(",2*i,"*pi*",timevar,"/",period,"), which=c(",which,"))",
                        " + fe( cos(",2*i,"*pi*",timevar,"/",period,"), which=c(",which,"))")
        }
    }
    
    as.formula(f, env = .GlobalEnv)
}

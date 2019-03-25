################################################################################
### Author: Sebastian Meyer [sebastian *.* meyer *a*t* ifspm *.* uzh *.* ch]
### Time-stamp: <[SI.R] 2015-10-06 22:50 (CEST) by SM>
### Project: Bug report by George Wood by e-mail on 28 September 2015
###
### as.epidata.data.frame() did not work for SI data (with missing tR.col)
### in surveillance 1.9-1. To be fixed in surveillance 1.10-0
################################################################################

devtools::load_all("~/Projekte/surveillance/pkg")
##library("surveillance")

## example using the hagelloch data
data("hagelloch")
head(hagelloch.df)

## conversion using the data.frame-method of as.epidata()
## does not work for SI-type data with missing 'tR.col'
hagelloch <- as.epidata(hagelloch.df,
  t0 = 0, tI.col = "tI", #tR.col = "tR",
  id.col = "PN", coords.cols = c("x.loc", "y.loc"),
  f = list(household    = function(u) u == 0,
           nothousehold = function(u) u > 0),
  w = list(c1 = function (CL.i, CL.j) CL.i == "1st class" & CL.j == CL.i,
           c2 = function (CL.i, CL.j) CL.i == "2nd class" & CL.j == CL.i),
  keep.cols = c("SEX", "AGE", "CL"))

##Error in beginBlock[eventblock + 1L]:endBlock[eventblock + 1L] (of epidata.R#245) :
##  NA/NaN Argument

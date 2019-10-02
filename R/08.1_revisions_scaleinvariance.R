###############################################################################
## This script aims to provide insight into how using trace as our means
## of a scalar PV value may be problematic due to scale invariance
##
##
## Created: September 27, 2019
###############################################################################


## goal: with our 

# Outline:
  ##load in response variables 
  ##check the scales of the variables
  ##what happens if seechi disk*1000?

#load in extrapolation data
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())

##how similar are the scales currently - very 
summary(Y)

## pulling code from '02.2_extrapolate_functions.R' to calculate 
  ## cov mat and then the trace

trace <- apply(Lambda.var, 3, trace_fun)


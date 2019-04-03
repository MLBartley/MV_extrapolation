###############################################################################
## This script aims to use the model fit data to obtain conditional 
## extrapolation measures for one RV (here, Total Nitrogen). 
##
## Created: November 16(ish), 2018
## Updated 1: Feb 25 - updated extrapolate_cond function to draw from [y_in|Y]
## rather than calculate \mu of [y_in|Y] 
###############################################################################



load(file = "./rdata-data/list-into-extrap.Rdata")
list2env(savelist,globalenv())

source("./R/02.2_extrapolate_functions.R")

library(mvtnorm) #needed in extrapolate_cond function

extrapolate.cond <- extrapolate_cond(X = X, Y = Y[, -5], Beta = Beta, Sigma = Sigma,
                                 Sampled = Sampled, Leverage = Leverage,
                                 link = 'none', Cond.index = 2)

dim(extrapolate.cond)
summary(extrapolate.cond) # still has 142 NAs - to check later 
which.max(extrapolate.cond) #check lake 6592 (IWS_lk_ratio?)

cutoffs.cond <- cutoffs_UV(extrap = extrapolate.cond, Sampled = Sampled, S = nrow(X))
  
 save(cutoffs.cond, file = "./rdata-data/extrap_TNonly.Rdata")

# ##Parallelize?###
# 
# extrap.cond.par <- extrapolate_cond_parallel(par.ind = 2, X = X, Y = Y[, -5], Beta = Beta, Sigma = Sigma,
#                                               Sampled = Sampled, Leverage = Leverage,
#                                               link = 'none', Cond.index = 2)
# 
# par.indx <- as.vector(1:length(Beta[1, 1,]))
# 
# extrap.cond.par <- sapply(par.indx,  FUN = extrapolate_cond_parallel,
#                             X = X, Y = Y[, -5], Beta = Beta, Sigma = Sigma,
#                             Sampled = Sampled, Leverage = Leverage,
#                             link = 'none', Cond.index = 2)
# x
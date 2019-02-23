###############################################################################
## This script aims to use the MCMC output (model fit with all observed lakes)
## with the extrapolate function to (1) obtain the prediction variances and 
## relative prediction variances of all observed, partially observed, and 
## unobserved lake locations, and (2) obtain the Extrapolation Index values 
## using the various cutoffs outlined in the Manuscript.
##
## Created: September 2018
## Updated 1: re-ran with updated cutoff code (<= not just <)
###############################################################################



### extrapolation work carried over/cleaned up from RUNALL_extrapolate_20180810.R
library(dplyr)
library(forcats)
library(magrittr)

load("./rdata-data/runAll_180808.Rdata") 

source("./R/03.3_RUNALL_eda_20180810.R") #gets leverage points and influential points
source("./R/02.2_extrapolate_functions.R") #gets useful functions

X <- rbind(runALL$data$X, runALL$data$Xtest)
LL <- rbind(runALL$data$LLxtrain, runALL$data$LLxtest)


Y <- as.data.frame(rbind(runALL$data$Y, runALL$data$Ytest))
Y[dim(Y)[1]:dim(X)[1], ] <- Y[8300, ] #doesn't have full 'missing' Y length 
Y <- Y %>% mutate(missing = apply(!is.na(Y), 1, sum))


Beta <- runALL$beta
Sigma <- runALL$Sigma 
  

Sampled <- 1:nrow(runALL$data$X)
rm(runALL)
 
Leverage <- leveragePoints[[1]] 

savelist <- list(X = X, LL = LL, Y = Y, Beta = Beta, Sigma = Sigma,
                 Sampled = Sampled, Leverage = Leverage)
 save(savelist, file = "./rdata-data/list-into-extrap.Rdata")

rm(leveragePoints, leverage_cutoff, hat, K, n, n.holdout, i)

LAGOS_extrapolate <- extrapolate(X = X, Beta = Beta, Sigma = Sigma,
                                 Sampled = Sampled, Leverage = Leverage,
                                 link = 'none')

 save(LAGOS_extrapolate, file = "./rdata-data/extrap_full.Rdata")
# load(file = "./rdata-data/extrap_full")

## Visualize - see more in 05.1_RUNALL_visualize_... file

plot(LAGOS_extrapolate$trace,  ylim = c(0, .03),
     col = (LAGOS_extrapolate$extrapolate$EC_95quantile[1, ])+1)

plot(LAGOS_extrapolate$determ * 10^15, ylim = c(-0.0001, 0.0001))


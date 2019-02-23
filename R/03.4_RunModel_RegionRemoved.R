###############################################################################
## This script aims to explore the efficacy of these MV extrapolation methods  
## and measures by purposefully removing a region of covariate space from the
## sampled lakes and refitting the model without those values. 

## Created: Januay 11, 2019
## Updated 1: 
###############################################################################

load(file="./rdata-data/runAll_180808.Rdata")			

## need to replace some of the observed data with NA 
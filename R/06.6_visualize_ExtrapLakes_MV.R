###############################################################################
## This script aims to provide summary statistics and visuals for lakes
## identified as extrapolations under the 95% cutoff value
## (least conservative, most extrapolations)
##
## Created: March 29, 2019
## Updated 1: 
###############################################################################

#load in extrapolation data
load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())

#isolate which lakes are extrapolation - 95% cutoff


## summary statistcs


## violin plots for each covariate/WQ variable
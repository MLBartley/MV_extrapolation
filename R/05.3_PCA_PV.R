###############################################################################
## This script aims to obtain explore a PCA analysis of the Extrapolation 
## Index values obtained from PV Measures on all observed lake data. 
##
## Created: January 11, 2019
## Updated 1: 
###############################################################################

library(ggbiplot)

load("./rdata-data/extrap_values_20180927") #extrapolation index values
load(file = "./rdata-data/extrap_full") #contains more extrapolation info
load(file = "./rdata-data/list-into-extrap.Rdata") #contains covariates and data

## what are we using in our PCA analysis? Don't want categorical variables
summary(savelist$X)

pca.data <- na.omit(cbind(LAGOS_extrapolate$trace, savelist$X[, -1]))


lakes.pca <-  prcomp(pca.data, center = TRUE,scale. = TRUE)
summary(lakes.pca)
str(lakes.pca)

ggbiplot(lakes.pca)

## add ExtrapIndex grouping

ggbiplot(lakes.pca, ellipse = T, groups = as.factor(na.omit(gg_dat[[1]]$nf_trace)))

## what about other PCA choices?
ggbiplot(lakes.pca, choices = c(3, 4),
         ellipse = T, groups = as.factor(na.omit(gg_dat[[1]]$nf_trace)))


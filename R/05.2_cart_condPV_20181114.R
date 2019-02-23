###############################################################################
## This script aims to obtain the CART model fit for TN only
## to create the visuals needed for reporting/manuscript. 
##
## Created: Decemer 02, 2018
## Updated 1: 
###############################################################################


#load covariates, data, etc
load(file = "./rdata-data/list-into-extrap.Rdata")
list2env(savelist,globalenv())
load("./rdata-data/extrap_TNonly.Rdata")

# library(rattle)
library(rpart)
# library(rpart.plot)
# library(maptree)
# 

names <- names(cutoffs.cond$cond.extrap)

fit_binary_TN <- list()
fit_numeric_TN <- list()

for (i in seq(1, length(names))) {
  
  dat <- t(cutoffs.cond$cond.extrap[[names[i]]])
  colnames(dat) <- c("IVH_binary", "IVH_range")
  
  IVH <- as.data.frame(cbind(dat, X))
  
  fit_binary_TN[[names[i]]] <- rpart(IVH_binary ~ . - IVH_range, data = IVH, 
                                  method = "class")
  fit_numeric_TN[[names[i]]] <- rpart(IVH_range ~ . - IVH_binary, data = IVH, 
                                     method = "anova")
  
  
}

savelist<- list(fit_binary_TN = fit_binary_TN, 
                fit_numeric_TN = fit_numeric_TN,
                CART.names = names)
save(savelist, file = "./rdata-data/CART_condPV_list.Rdata")

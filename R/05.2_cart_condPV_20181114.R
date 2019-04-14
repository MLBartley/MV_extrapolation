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
load("./rdata-data/extrap_values_TNonly")

# library(rattle)
library(rpart)
# library(rpart.plot)
# library(maptree)
# 

# names <- names(cutoffs.cond$cond.extrap)
names <- sort(colnames(gg_TN[[1]])[-(1:2)])
names <- names[-c(3:4)] #max is all predicted no need for CART
X <- cbind(gg_TN[[1]][1:2], X[, -1])

fit_binary_TN <- list()
fit_numeric_TN <- list()

for (i in seq(1, length(names), by =2)) {
  
  dat <- gg_TN[[1]][names[i:(i+1)]]
  colnames(dat) <- c("IVH_binary", "IVH_range")
  
  IVH <- cbind(dat, X)
  
  fit_binary_TN[[names[i]]] <- rpart(IVH_binary ~ . - IVH_range, 
                                     data = IVH, 
                                  method = "class", 
                                  model = TRUE, 
                                  control = rpart.control(cp = 0.0001))
  fit_numeric_TN[[names[i+1]]] <- rpart(IVH_range ~ . - IVH_binary, data = IVH, 
                                     method = "anova")
  
  
}

savelist<- list(fit_binary_TN = fit_binary_TN, 
                fit_numeric_TN = fit_numeric_TN,
                CART.names = names)
save(savelist, file = "./rdata-data/CART_condPV_list.Rdata")


###############################################################################
## This script aims to obtain the CART model fit for all lakes
## to create the visuals needed for reporting/manuscript. 
##
## Created: Decemer 02, 2018
## Updated 1: 
###############################################################################

# source("./R/04.1_extrapolate_PV.R")

  load(file = "./rdata-data/list-into-extrap.Rdata") #4.01
    list2env(savelist,globalenv())


  load("./rdata-data/extrap_values_20180927") #made later in 6.01?

library(rattle)
library(rpart)
library(rpart.plot)
library(maptree)

## CART model on IVH results

str(gg_dat)
str(X)

names <- sort(colnames(gg_dat[[1]])[-(1:2)])
X <- cbind(gg_dat[[1]][1:2], X[, -1])

fit_binary <- list()
fit_numeric <- list()

for (i in seq(1, length(names), by =2)) {
  
  dat <- gg_dat[[1]][names[i:(i+1)]]
  colnames(dat) <- c("IVH_binary", "IVH_range")
  
  IVH <- cbind(dat, X)
  
  fit_binary[[names[i]]] <- rpart(IVH_binary ~ . - IVH_range, data = IVH, 
                                  method = "class")
  fit_numeric[[names[i+1]]] <- rpart(IVH_range ~ . - IVH_binary, data = IVH, 
                                     method = "anova")
  
  
}

savelist<- list(fit_binary = fit_binary, fit_numeric = fit_numeric, 
                CART.names = names)
save(savelist, file = "./rdata-data/CART_PV_list_addLatLong.Rdata")

# ##binary 
#1 "levmax_determ"         
#2 "levmax_determ_noise"  
#3  "levmax_trace"              
#4 "max_determ"          
#5  "max_determ_noise"     
#6  "max_trace"               
#7 "nf_determ"           
#8  "nf_determ_noise"      
#9     "nf_trace"                    
#10  "nn_determ"          
#11    "nn_determ_noise"      
#12    "nn_trace"          


##check resulting models 
fit_nf_determ <- fit_binary[[7]]
fit_nn_tr <- fit_binary[[12]]

fit <- fit_nf_determ ## change to check diff versions

printcp(fit)	#display cp table
plotcp(fit)	#plot cross-validation results
# rsq.rpart(fit)	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(fit)	#print results
summary(fit)	#detailed results including surrogate splits
plot(fit)	#plot decision tree
text(fit)	#label the decision tree plot

fit.prune <- prune(fit, cp = 0.02)


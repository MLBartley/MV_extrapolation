###############################################################################
## This script aims to obtain the CART model fit for all lakes
## to create the visuals needed for reporting/manuscript. 
##
## Created: Decemer 02, 2018
## Updated 1: 
###############################################################################

source("./R/04.1_extrapolate_PV_20180907.R")
load("./rdata-data/extrap_values_20180927")

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

savelist<- list(fit_binary = fit_binary, fit_numeric = fit_numeric, CART.names = names)
save(savelist, file = "./rdata-data/CART_PV_list_addLatLong.Rdata")


##check resulting models 
fit <- fit_binary[[12]]

printcp(fit)	#display cp table
plotcp(fit)	#plot cross-validation results
# rsq.rpart(fit)	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(fit)	#print results
summary(fit)	#detailed results including surrogate splits
plot(fit)	#plot decision tree
text(fit)	#label the decision tree plot

fit.prune <- prune(fit, cp = 0.02)

# 
# draw.tree(fit,cex=1)
# prp(fit, type = 0, extra = 106, cex = 1)
# rpart.plot(fit,                    # middle graph
#            extra = 101,          # show fitted class, probs, percentages
#            box.palette = c("#E41A1C","#A6CEE3"), # color scheme
#            branch.lty = 3,       # dotted branch lines
#            shadow.col = "gray",  # shadows under the node boxes
#            nn = F, 
#            cex = 1,
#            # tweak = 1,
#            compress = F,
#            ycompress = F,
#            gap = 0, 
#            space= 0 
#            # main = "99% cutoff + determinant"
#            )   
# fancyRpartPlot(fit)
# 
# # pdf("figures/nfd_CART.pdf",  width = 34, height = 15)
# prp(fit, type = 0, extra = 106)
# dev.off()
# 
# # post(fit, file=)	#create postscript plot of decision tree
# 
# fit2 <- fit_numeric[[7]]
# 
# printcp(fit2)	#display cp table
# plotcp(fit2)	#plot cross-validation results
# rsq.rpart(fit2)	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
# print(fit2)	#print results
# summary(fit2)	#detailed results including surrogate splits
# plot(fit2)	#plot decision tree
# text(fit2)	#label the decision tree plot
# prp(fit2, type = 0)
# post(fit2, file=)	#create postscript plot of decision tree

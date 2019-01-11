###############################################################################
## This script aims to use the CART model fit for ATN only
## to create the visuals needed for reporting/manuscript. 
##
## Created: December 02, 2018
## Updated 1: 
###############################################################################
load("./rdata-data/CART_condPV_list.Rdata")
list2env(savelist,globalenv())

library(rattle)
library(rpart)
library(rpart.plot)
library(maptree)

whichplot_fit_binary <- fit_binary_TN[c(3)]

 pdf("figures/binary_CART_UV_nf.pdf",  width = 34, height = 30)

for (i in  1:length(whichplot_fit_binary)) { #won't work for max or lev max
 rattle::fancyRpartPlot(whichplot_fit_binary[[i]], 
                        main = names(whichplot_fit_binary[i]), 
                        palettes = c("Reds", "Blues"))
  
  rpart.plot(whichplot_fit_binary[[i]],
             extra = 101,          # show fitted class, probs, percentages
             box.palette = c("#E41A1C","#A6CEE3"), # color scheme
             branch.lty = 3,       # dotted branch lines
             # shadow.col = "gray",  # shadows under the node boxes
             nn = F,
             cex = 1)
}
dev.off()

# 
# draw.tree(fit,cex=1)
# prp(fit, type = 0, extra = 106, cex = 1)
# rpart.plot(whichplot_fit_binary[[1]],                    # middle graph
#            extra = 101,          # show fitted class, probs, percentages
#            box.palette = c("#E41A1C","#A6CEE3"), # color scheme
#            branch.lty = 3,       # dotted branch lines
#            # shadow.col = "gray",  # shadows under the node boxes
#            nn = F, 
#            cex = 1,
#            # tweak = 1,
#            compress = F,
#            ycompress = F,
#            gap = 0, 
#            space= 0 
#            # main = "99% cutoff + determinant"
# )   
# fancyRpartPlot(whichplot_fit_binary[[1]], palettes = c("Reds", "Blues"))
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

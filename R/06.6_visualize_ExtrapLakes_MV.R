###############################################################################
## This script aims to provide summary statistics and visuals for lakes
## identified as extrapolations under the 95% cutoff value
## (least conservative, most extrapolations)
##
## Created: March 29, 2019
## Updated 1: 
###############################################################################

library(magrittr)
library(ggplot2)

#load in extrapolation data
load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())
load(file = "./rdata-data/extrap_values_20180927") #created in 06.1

load("./rdata-data/AllDataList_180627v2.Rdata") #created in cluster.datamanage.R
load("./rdata-data/MissingDataList_180815.Rdata")


##bring Lake IDs forward
gg_dat2 <-  cbind(gg_dat[[1]], #extrapolation info
                  lakeID, #lake id
                  X, 
                  Y) #logY 


#isolate which lakes are extrapolation - 95% cutoff

table(factor(gg_dat2$nf_trace))

extrapolated.all <- which(factor(gg_dat2$nf_trace) == 0)
extrapolated.pred <- extrapolated.all[which(extrapolated.all > max(Sampled))]

whichID.extrap.pred <- gg_dat2$lakeID[extrapolated.pred]

gg_dat2$nft_extrap <- "predicted"
gg_dat2$nft_extrap[extrapolated.pred] <- "extrapolated"

## summary statistcs

summary(gg_dat2[extrapolated.pred,])




## hist/density plots for X variables

Xnames <- colnames(X)[-1] #-1 removes mean vector of 1s

# Xggplot <- list()

pdf("./figures/extrap_violins.pdf")
for(i in 1:length(Xnames)) {
  var <- Xnames[i]
  
q <- ggplot(gg_dat2, aes(x = "All", y = gg_dat2[, var])) +
    geom_violin() +
    geom_point(aes(color="All"), position = "jitter") +
    geom_violin(data=gg_dat2, aes(x = nft_extrap, y = gg_dat2[, var])) +
    geom_point(data=gg_dat2, aes(x = nft_extrap, y = gg_dat2[, var], color = nft_extrap), 
               position = "jitter") +
    scale_color_manual(values = c("black","#F8766D","#619CFF")) +
    theme_minimal(base_size = 16) +
    ylab(label = var) + 
    theme(axis.title.x = element_blank(), legend.title = element_blank())
print(q)

}
dev.off()

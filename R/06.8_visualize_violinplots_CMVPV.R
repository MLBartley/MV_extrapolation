###############################################################################
## This script aims to provide summary statistics and visuals for lakes
## identified as extrapolations under the 95%  and 99% cutoff value 
## TN ONLY!
##
## Created: March 29, 2019
## Updated 1: scaled data to make figures align on one plot
###############################################################################

library(magrittr)
library(ggplot2)
library(reshape2)


#load in extrapolation data
load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())
load(file = "./rdata-data/extrap_values_TNonly") #created in 06.2

load("./rdata-data/AllDataList_180627v2.Rdata") #created in cluster.datamanage.R
load("./rdata-data/MissingDataList_180815.Rdata")

X.scaled <- as.data.frame(scale(X))

##bring Lake IDs forward
gg_dat2 <-  cbind(gg_TN[[1]], #extrapolation info
                  lakeID, #lake id
                  X.scaled,
                  Y) #logY 

gg_dat_unscaled <- cbind(gg_TN[[1]], #extrapolation info
                         lakeID, #lake id
                         X,
                         Y)

#isolate which lakes are extrapolation - 95% cutoff

table(factor(gg_dat2$nf_extrap))

extrapolated.all <- which(factor(gg_dat2$nf_extrap) == 0)

Unsampled_TN <- which(is.na(Y$tn_combined))

extrapolated.pred <-  which(extrapolated.all %in% Unsampled_TN)

gg_dat2$nf_extrapindex <- "predicted"
gg_dat2$nf_extrapindex[extrapolated.all[extrapolated.pred]] <- "extrapolated"

table(gg_dat2$nf_extrapindex)



# 99% cutoff

table(factor(gg_dat2$nn_extrap))

extrapolated.all <- which(factor(gg_dat2$nn_extrap) == 0)
extrapolated.pred <-  which(extrapolated.all %in% Unsampled_TN)


gg_dat2$nn_extrapindex <- "predicted"
gg_dat2$nn_extrapindex[extrapolated.all[extrapolated.pred]] <- "extrapolated"

table(gg_dat2$nn_extrapindex)

xtable(gg_dat_unscaled[extrapolated.all[extrapolated.pred], colnames(X)[-1]])

# # levmax cutoff
# 
# table(factor(gg_dat2$levmax_extrap))
# 
# extrapolated.all <- which(factor(gg_dat2$levmax_extrap) == 0)
# extrapolated.pred <- which(extrapolated.all %in% Unsampled_TN)
# 
# 
# gg_dat2$levmax_ext <- "predicted"
# gg_dat2$levmax_ext[extrapolated.all[extrapolated.pred]] <- "extrapolated"
# 
# table(gg_dat2$levmax_ext)
# 
# xtable(gg_dat_unscaled[extrapolated.pred, colnames(X)[-1]])
# 

## reformat data for all in one plot

gg_dat_melt <- melt(gg_dat2, 
                    id.vars = c("lakeID", "nf_extrapindex", "nn_extrapindex"), 
                    measure.vars = names(X.scaled)[-c(1, 17, 18)])



## summary statistcs

summary(gg_dat2[extrapolated.pred,])




## hist/density plots for X variables

Xnames <- colnames(X)[-1] #-1 removes mean vector of 1s

# Xggplot <- list()




## all covariates scaled on one figure




## parallel coordinates
library(GGally)



#99%

subset_nn <- subset(gg_dat2, nn_extrapindex == "extrapolated")
subset_subset <- sample(x = 1:(dim(subset_nn)[1]), size = 10)

ggparcoord(subset_nn[subset_subset, ], 
           # scale = "uniminmax", 
           columns=c(13:27), 
           alphaLines = 0.3, 
           showPoints = T, 
           mapping=aes(color=as.factor(lakeID)),  
           order = "skewness") +
  geom_point(aes(size = 3)) +
  
  ylim(c(-3, 4)) +
  
  
  geom_violin(data = gg_dat_melt, 
              aes_string(group = "variable"), 
              scale = "width", 
              fill = NA) +
  # geom_point(data = gg_dat_melt, aes(y = "value", group = "variable", color= "#F8766D"), 
  #            position = "jitter", 
  #            alpha = 0.35) +
  theme_minimal(base_size = 40) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
  # ylab(label = var) + 
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank())  +
  ggtitle("TN Extrapolated Lakes - 99% Cutoff") +
  theme(legend.position="none")
# 
ggsave("figures/figureS2.eps",
       device=cairo_ps,
       width = 860, height = 573,
       units = "mm")

ggsave("figures/violin99_CMVPV.eps",
       device=cairo_ps,
       width = 860, height = 573,
       units = "mm")

#levmax
ggparcoord(subset(gg_dat2, levmax_ext == "extrapolated"), 
           # scale = "uniminmax", 
           columns=c(13:29), 
           alphaLines = 0.3, 
           showPoints = T, 
           mapping=aes(color=as.factor(lakeID))) +
  
  ylim(c(-3, 4)) +
  
  
  geom_violin(data = gg_dat_melt, 
              aes_string(group = "variable"), 
              scale = "width", 
              fill = NA) +
  # geom_point(data = gg_dat_melt, aes(y = "value", group = "variable", color= "#F8766D"), 
  #            position = "jitter", 
  #            alpha = 0.35) +
  theme_minimal(base_size = 40) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
  ylab(label = "Scaled Value") +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank()) +
  ggtitle("TN Extrapolated Lakes - Leverage Cutoff") +
    theme(legend.position="none")







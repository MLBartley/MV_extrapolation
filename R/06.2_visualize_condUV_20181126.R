###############################################################################
## This script aims to use the conditional extrapolation measures for TN 
## to create the visuals needed for reporting/manuscript. 
##
## Created: November 26, 2018
## Updated 1: 
###############################################################################

library(ggplot2)
library(ggpubr)
library("gridExtra")
library(maps)
library(magrittr)

load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())
load(file = "./rdata-data/extrap_TNonly.Rdata")


## Spatial Visualize

## want to color dots by Full/Partial/Missing Y data
x = LL$nhd_long
y = LL$nhd_lat
# miss <- c(Y$missing, rep(0, 723 - 113))

miss <- as.factor(Y$missing)
miss.f <- forcats::fct_collapse(miss, full = c("4"), 
                                partial = c("1", "2", "3"), missing = c("0"))

miss.TN <- as.factor(is.na(Y$tn_combined)) %>% 
  forcats::fct_collapse( full = c("FALSE"), 
                        missing = c("TRUE"))

states <- map_data("state") %>% 
  subset(region %in% c("minnesota", "iowa", "missouri", 
                       "wisconsin", "illinois", "michigan", 
                       "indiana", "ohio", "pennsylvania", 
                       "new jersey", "new york", "vermont", 
                       "new hampshire", "maine", "connecticut",
                       "rhode island", "massachusetts", "kentucky", 
                       "west virginia", "virginia", "maryland"))
gg_TN <- list()

gg_TN[[1]] <- cbind(LL, (t(cutoffs.cond$cond.extrap$EC_max)),
                     (t(cutoffs.cond$cond.extrap$EC_levmax)),
                     (t(cutoffs.cond$cond.extrap$EC_95quantile)), 
                     (t(cutoffs.cond$cond.extrap$EC_99quantile)))
colnames(gg_TN[[1]])[3:4] <- c("max_extrap", "max_rel" ) 
colnames(gg_TN[[1]])[5:6] <- c("levmax_extrap", "levmax_rel") 
colnames(gg_TN[[1]])[7:8] <- c("nf_extrap", "nf_rel") 
colnames(gg_TN[[1]])[9:10] <- c("nn_extrap", "nn_rel") 

gg_TN[[2]] <- cutoffs.cond$cutoffs

Unsampled_TN <- which(is.na(Y$tn_combined))

 # save(gg_TN, file = "./rdata-data/extrap_values_TNonly")

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Paired")
names(myColors) <- levels(gg_TN$determ)

missColors <- brewer.pal(3, "Dark2")
names(missColors) <- levels(miss.f)

gg_TN <- gg_TN[[1]]


## common arguments for following plots - only have to alter here
th.paper <- theme(text = element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), 
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(), 
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())

th.poster <- theme(text = element_text(size=80),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), 
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(), 
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

pt.size.paper <- geom_point(size = 0.1)

states.lines <- geom_polygon(data = states, aes(x = long, y = lat, group = group),
                             fill = NA, color = "black")


# maps of full, missing data locations - TN only
datamiss_TN <- ggplot(gg_TN, aes(x = nhd_long, y = nhd_lat, color = miss.TN)) +
  scale_colour_manual(name = "Data Status",
                      values = missColors[c(1, 3)], 
                      breaks = c("missing", "full"),
                      labels = c("TN unobserved",  
                                 "TN observed")) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper + 
  theme(legend.position="bottom")

datamiss_TN

## 

mt <- ggplot(na.omit(gg_TN[Unsampled_TN, ]), 
             aes(x = nhd_long, y = nhd_lat, 
                 color = factor(max_extrap))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C", "#A6CEE3"),
                      limits=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))   +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
mt


##cutoff: -leverage max

lmt <- ggplot(na.omit(gg_TN[Unsampled_TN, ]), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(levmax_extrap))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +  
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
lmt



## cutoff: 95%

nft <- ggplot(na.omit(gg_TN[Unsampled_TN, ]), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nf_extrap))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nft



## cutoff: 99%

nnt <- ggplot(na.omit(gg_TN[Unsampled_TN, ]), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nn_extrap))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nnt


rm(gg_TN)



ggarrange(mt, lmt, nnt, nft,
          labels = c("A. Maximum Cutoff",
                     "B. Leverage Cutoff",
                     "C. 99% Cutoff",
                     "D. 95% Cutoff"),
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "bottom")

ggsave("./figures/extrap_TNonly.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)


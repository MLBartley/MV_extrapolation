###############################################################################
## This script aims to provide summary statistics and visuals for lakes
## identified as extrapolations under the four cutoff value
## (least conservative, most extrapolations)
##
## USES NUMERIC EXTRAPOLATION INDEX!!
##
## Created: October 6, 2019
##
###############################################################################
library(ggplot2)
library(ggpubr)
library("gridExtra")
library(maps)
library(scales)


load(file = "./rdata-data/extrap_TNonly.Rdata")#created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())


## Spatial Visualize

## want to color dots by Full/Partial/Missing Y data
x = LL$nhd_long
y = LL$nhd_lat
# miss <- c(Y$missing, rep(0, 723 - 113))

miss <- as.factor(Y$missing)
miss.f <- forcats::fct_collapse(miss, full = c("4"), 
                                partial = c("1", "2", "3"), missing = c("0"))


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


gg_TN_pred <- gg_TN[[1]][Unsampled_TN,]





## common arguments for following plots - only have to alter here
th.paper <- theme(text = element_text(size=40),
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

pt.size.paper <- geom_point(size = 3)
# pt.size.paper <- geom_point(size = 4) #pdf file


states.lines <- geom_polygon(data = states, aes(x = long, y = lat, group = group),
                             fill = NA, color = "black")


##plot extraplation location using NUMERIC Extrapolation Index

##gotta make sure binary and numeric results line up
table(gg_TN_pred$max_extrap)
check_numeric <- ifelse(gg_TN_pred$max_rel > 1, 0, 1)
table(check_numeric)

table(gg_TN_pred$levmax_extrap)
check_numeric <- ifelse(gg_TN_pred$levmax_rel > 1, 0, 1)
table(check_numeric)


table(gg_TN_pred$nf_extrap)
check_numeric <- ifelse(gg_TN_pred$nf_rel > 1, 0, 1)
table(check_numeric)


table(gg_TN_pred$nn_extrap)
check_numeric <- ifelse(gg_TN_pred$nn_rel > 1, 0, 1)
table(check_numeric)



TN_pred_na.omit <- na.omit(gg_TN_pred)

## OK we need to add coded variable to indicate 
## shape: Which cutoff choice first marks as extrapolation
## color: Is lake an extrapolation under trace/determ/both
## other: which are ALWAYS predictions

##color - when added

## 0 = prediction
## 1 = added with max cutoff
##  2 = added with maxlev cutoff
## 3 = added with 99%
## 4 = added with 95%


TN_pred_na.omit$color_det <- ifelse(TN_pred_na.omit$max_extrap == 0, 1, 
                                        ifelse(TN_pred_na.omit$levmax_extrap == 0, 2, 
                                               ifelse(TN_pred_na.omit$nn_extrap == 0, 3,
                                                      ifelse(TN_pred_na.omit$nf_extrap == 0, 4, 0))
                                        )
)


##UPDATE: changing from color gradient to size gradient 
## so only color legend needed

colors <- c("#A6CEE3", "#e41a3e", "#e48e1a", "#e4e21a", "#9f122b")

TN_all <- ggplot(TN_pred_na.omit) +
  geom_point(#data = subset(gg_dat_pred_na.omit, color_det == 0),
    aes(nhd_long,
        nhd_lat,
        color = as.factor(color_det + 1),
        alpha = as.factor(color_det + 1),
        shape =  as.factor(color_det + 1), 
        size = as.factor(color_det + 1))) +
  scale_shape_manual(name = "Extrapolation Cutoff",
                     labels = c("Prediction", "Leverage Cutoff", "99% Cutoff", "95% Cutoff"), 
                     values=c(16, 15, 17, 18))+
  scale_alpha_manual(name = "Extrapolation Cutoff",
                     labels = c("Prediction", "Leverage Cutoff", "99% Cutoff", "95% Cutoff"), 
                     values=c(.8, 1, 1, 1)) +
  scale_size_manual(name = "Extrapolation Cutoff",
                    labels = c("Prediction", "Leverage Cutoff", "99% Cutoff", "95% Cutoff"), 
                    values=c(5, 8, 8, 8)) +
  scale_colour_manual(name = "Extrapolation Cutoff",
                      values = colors[1:5], 
                      breaks = c( 1, 3, 4, 5),
                      labels = c("Prediction", "Leverage Cutoff", "99% Cutoff", "95% Cutoff")) +
states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=50)) +
  guides(color = guide_legend(override.aes = list(size = 12))) + 
  theme(legend.position="bottom")

TN_all
ggsave("./figures/Fig5_revisions.eps", 
       device=cairo_ps, #ensures the transparancy stays
       width = 860, height = 573,
       units = "mm")
  


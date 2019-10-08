###############################################################################
## This script aims to provide summary statistics and visuals for lakes
## identified as extrapolations under the 95% cutoff value
## (least conservative, most extrapolations)
##
## USES NUMERIC EXTRAPOLATION INDEX!!
##
## Created: September 25, 2019
## Updated 1: scaled data to make figures align on one plot
###############################################################################
library(ggplot2)
library(ggpubr)
library("gridExtra")
library(maps)
library(scales)


load(file = "./rdata-data/extrap_full") #created in 04.1
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
gg_dat <- list()

gg_dat[[1]] <- cbind(LL, (t(LAGOS_extrapolate$extrapolate$EC_max)),
                     (t(LAGOS_extrapolate$extrapolate$EC_levmax)),
                     (t(LAGOS_extrapolate$extrapolate$EC_95quantile)), 
                     (t(LAGOS_extrapolate$extrapolate$EC_99quantile)))
colnames(gg_dat[[1]])[3:8] <- c("max_trace", "max_determ", "max_determ_noise",
                                "max_trace_n", "max_determ_n", "max_determ_noise_n" ) 
colnames(gg_dat[[1]])[9:14] <- c("levmax_trace", "levmax_determ", "levmax_determ_noise", 
                                 "levmax_trace_n", "levmax_determ_n", "levmax_determ_noise_n") 
colnames(gg_dat[[1]])[15:20] <- c("nf_trace", "nf_determ", "nf_determ_noise", 
                                  "nf_trace_n", "nf_determ_n", "nf_determ_noise_n") 
colnames(gg_dat[[1]])[21:26] <- c("nn_trace", "nn_determ", "nn_determ_noise", 
                                  "nn_trace_n", "nn_determ_n", "nn_determ_noise_n") 

gg_dat[[2]] <- LAGOS_extrapolate$cutoffs


#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Paired")
names(myColors) <- levels(gg_dat$determ)


gg_dat_pred <- gg_dat[[1]][-Sampled,]

gg_dat <- cbind(gg_dat[[1]], Y)




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
table(gg_dat_pred$max_trace)
table(gg_dat_pred$max_determ)
check_numeric <- ifelse(gg_dat_pred$max_trace_n > 1, 0, 1)
table(check_numeric)
head(sort(gg_dat_pred$max_trace_n, decreasing = T))

table(gg_dat_pred$levmax_trace)
table(gg_dat_pred$levmax_determ)
check_numeric <- ifelse(gg_dat_pred$levmax_trace_n > 1, 0, 1)
table(check_numeric)
head(sort(gg_dat_pred$levmax_trace_n, decreasing = T))


table(gg_dat_pred$nf_trace)
table(gg_dat_pred$nf_determ)
check_numeric <- ifelse(gg_dat_pred$nf_trace_n > 1, 0, 1)
table(check_numeric)
head(sort(gg_dat_pred$nf_trace_n, decreasing = T))


table(gg_dat_pred$nn_trace)
table(gg_dat_pred$nn_determ)
check_numeric <- ifelse(gg_dat_pred$nn_trace_n > 1, 0, 1)
table(check_numeric)
head(sort(gg_dat_pred$nn_trace_n, decreasing = T))


## MVPV(trace) + max cutoff
mt <- ggplot(na.omit(gg_dat_pred), 
             aes(x = nhd_long, y = nhd_lat, 
                 color = (max_trace_n))) +
  scale_colour_gradient2(low = "#A6CEE3", 
                         high = "#E41A1C", midpoint = 1) +
  
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30))
mt

# ggsave("tp_bystate.pdf")

## MVPV(determinent) + max cutoff
# md <- ggplot(na.omit(gg_dat_pred), aes(x = nhd_long, y = nhd_lat, 
#                                        color = (max_determ_n))) +
#   scale_colour_gradient2(low = "#A6CEE3", 
#                          high = "maroon",
#                          midpoint = 1) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper 
# md

# ggsave("tp_bystate.pdf")


##cutoff: -leverage max

lmt <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = log(levmax_trace_n))) +
  scale_colour_gradient2(low = "#A6CEE3",
                         mid = "#A6CEE3",
                         high = "#E41A1C",
                         midpoint = 0) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30))
lmt

# ggsave("tp_bystate.pdf")

# lmd <- ggplot(na.omit(gg_dat), 
#               aes(x = nhd_long, y = nhd_lat, 
#                   color = (levmax_determ_n))) +
#   scale_colour_gradient2(low = "#A6CEE3", 
#                          high = "#E41A1C", midpoint = 1) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# lmd

# ggsave("figures/levmax_extrap.pdf")



## cutoff: 95%

nft <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = (log(nf_trace_n)))) +
  scale_colour_gradient2(low = "#A6CEE3",
                         mid = "#A6CEE3",
                         high = "#E41A1C",
                         midpoint = 0) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30))
nft

# ggsave("tp_bystate.pdf")

# nfd <- ggplot(na.omit(gg_dat_pred), aes(x = nhd_long, y = nhd_lat, 
#                                    color = log(nf_determ_n))) +
#   scale_colour_gradient2(low = "#A6CEE3",
#                          mid = "#A6CEE3",
#                          high = "#E41A1C",
#                          midpoint = 0) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nfd

# ggsave("figures/nfdeterm_extrap.pdf", 
# # width = 860, height = 573,
# units = "mm")


## cutoff: 99%

nnt <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = log(nn_trace_n))) +
  scale_colour_gradient2(low = "#A6CEE3",
                         mid = "#A6CEE3",
                         high = "#E41A1C",
                         midpoint = 0) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30))
nnt

# ggsave("tp_bystate.pdf")
# 
# nnd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat,
#                                    color = (nn_determ_n))) +
#   scale_colour_gradient2(low = "#A6CEE3", 
#                          high = "#E41A1C", midpoint = 1) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nnd

# ggsave("tp_bystate.pdf")

# nndn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nn_determ_noise))) +
#   scale_colour_manual(name = "ivh",values = myColors[1:2]) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nndn

# ggsave("tp_bystate.pdf")



ggarrange(mt, lmt, nnt, nft,  
          labels = c("A. Maximum Cutoff", 
                     "B. Leverage Cutoff", 
                     "C. 99% Cutoff", 
                     "D. 95% Cutoff"),
          ncol = 2, nrow = 2, 
          font.label = list(size = 30),
          common.legend = TRUE,
          legend = "bottom") 

ggsave("./figures/figureS3.eps", 
       width = 860, height = 573,
       units = "mm")





# ggarrange(md, lmd, nnd, nfd,  
#           labels = c("A. Maximum Cutoff", 
#                      "B. Leverage Cutoff", 
#                      "C. 99% Cutoff", 
#                      "D. 95% Cutoff"),
#           ncol = 2, nrow = 2, 
#           common.legend = TRUE, 
#           legend = "bottom")
# 
# ggsave("./figures/Fig3.eps", plot = last_plot(), device = "eps", path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE)

gg_dat_pred_na.omit <- na.omit(gg_dat_pred)

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

# gg_dat_pred_na.omit$color_tr <- 0
gg_dat_pred_na.omit$color_tr <- ifelse(gg_dat_pred_na.omit$max_trace == 0, 1, 
                                       ifelse(gg_dat_pred_na.omit$levmax_trace == 0, 2, 
                                              ifelse(gg_dat_pred_na.omit$nn_trace == 0, 3,
                                                     ifelse(gg_dat_pred_na.omit$nf_trace == 0, 4, 0))
                                       )
)


gg_dat_pred_na.omit$color_det <- ifelse(gg_dat_pred_na.omit$max_determ == 0, 1, 
                                       ifelse(gg_dat_pred_na.omit$levmax_determ == 0, 2, 
                                              ifelse(gg_dat_pred_na.omit$nn_determ == 0, 3,
                                                     ifelse(gg_dat_pred_na.omit$nf_determ == 0, 4, 0))
                                       )
)



##shape

# gg_dat_pred_na.omit$shape <


### ok now let's try to make a combined map adding layers with separate color schemes?

trace_all <- ggplot(gg_dat_pred_na.omit) +
  # scale_colour_gradient2(low = "#A6CEE3",
  #                        mid = "#A6CEE3",
  #                        high = "#E41A1C",
  #                        midpoint = 0) +
# geom_point(size = 2,
#            aes(nhd_long,
#                nhd_lat)) +
  ## add blue "prediction" colors
  geom_point(data = subset(gg_dat_pred_na.omit, color_tr == 0),
             aes(nhd_long,
             nhd_lat),
             color = "#A6CEE3",
             size = 5) +
  geom_point(data = subset(gg_dat_pred_na.omit, color_tr == 2),
             aes(nhd_long,
                 nhd_lat),
             color = "#E4E21A",
             size = 5) +
  geom_point(data = subset(gg_dat_pred_na.omit, color_tr == 3),
             aes(nhd_long,
                 nhd_lat,
             color = nn_trace_n),
             size = 5) +
  scale_color_gradient(low = "#eda45e",
                         high = "#9f5712") +
  geom_point(data = subset(gg_dat_pred_na.omit, color_tr == 4),
             aes(nhd_long,
                 nhd_lat,
             fill = nf_trace_n),
             size = 5, 
            shape = 21) +
  scale_fill_gradient(low = "#ed5e5f",
                       high = "#9f1214") +
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30))

trace_all
ggsave("./figures/figureTEST.eps", 
       width = 860, height = 573,
       units = "mm")

##UPDATE: changing from color gradient to size gradient 
## so only color legend needed

colors <- c("#A6CEE3", "#e41a3e", "#e48e1a", "#e4c01a", "#9f122b")

determ_all <- ggplot(gg_dat_pred_na.omit) +
  geom_point(#data = subset(gg_dat_pred_na.omit, color_det == 0),
             aes(nhd_long,
                 nhd_lat,
             color = as.factor(color_det + 1),
             shape =  as.factor(color_det + 1), 
             size = log(gg_dat_pred_na.omit$nf_determ_n - min(gg_dat_pred_na.omit$nf_determ_n) + 0.01))) +
  scale_shape_manual(name = "Extrapolation Cutoff",
                     labels = c("Prediction", "99% Cutoff", "95% Cutoff"), 
                     values=c(16, 15, 15, 15, 15))+
  scale_size_continuous(range = c(5,10), guide = 'none') +
  scale_colour_manual(name = "Extrapolation Cutoff",
                      values = colors[1:5], 
                      breaks = c( 1, 4, 5),
                      labels = c("Prediction", "99% Cutoff", "95% Cutoff")) +
  # geom_point(data = subset(gg_dat_pred_na.omit, color_det == 2),
  #            aes(nhd_long,
  #                nhd_lat),
  #            color = "#e41a3e",
  #            size = 5, 
  #            shape = 7) +
  # geom_point(data = subset(gg_dat_pred_na.omit, color_det == 3),
  #            aes(nhd_long,
  #                nhd_lat,
  #                color = nn_determ_n),
  #            size = 5, 
  #            shape = 15) +
  # scale_color_gradient(low = "#eda45e",
  #                      high = "#9f5712") +
  # geom_point(data = subset(gg_dat_pred_na.omit, color_det == 4),
  #            aes(nhd_long,
  #                nhd_lat,
  #                fill = nf_determ_n),
  #            size = 5, 
  #            shape = 22) +
  # scale_fill_gradient(low = "#edd35e",
  #                     high = "#9f8612") +
  states.lines + 
  coord_fixed(1.3) +
  th.paper + theme(legend.text=element_text(size=30)) +
  guides(color = guide_legend(override.aes = list(size = 10))) + 
  theme(legend.position="bottom")

determ_all
ggsave("./figures/figure_NEW.eps", 
       width = 860, height = 573,
       units = "mm")



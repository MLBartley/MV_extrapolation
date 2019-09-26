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

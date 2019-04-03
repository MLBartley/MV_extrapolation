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

load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())


## Spatial Visualize


#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Paired")
names(myColors) <- levels(gg_dat$determ)

missColors <- brewer.pal(3, "Dark2")
names(missColors) <- levels(miss.f)

gg_dat <- gg_dat[[1]]


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


## 

mt <- ggplot(na.omit(gg_dat), 
             aes(x = nhd_long, y = nhd_lat, 
                 color = factor(max_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
mt

# ggsave("tp_bystate.pdf")

# md <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, 
#                                   color = factor(max_determ))) +
#   scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction"))  +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# md

# ggsave("tp_bystate.pdf")

# mdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, 
#                           color = factor(max_determ_noise))) +
#   scale_colour_manual(name = "Extraplation",
#                       values = c("#E41A1C","#A6CEE3")) + 
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# mdn

# ggsave("tp_bystate.pdf")

##cutoff: -leverage max

lmt <- ggplot(gg_dat, 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(levmax_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +  
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
lmt

# ggsave("tp_bystate.pdf")

# lmd <- ggplot(na.omit(gg_dat), 
#               aes(x = nhd_long, y = nhd_lat, 
#                   color = factor(levmax_determ))) +
#   scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction"))  +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# lmd

# ggsave("figures/levmax_extrap.pdf")

# lmdn <- ggplot(na.omit(gg_dat), 
#                aes(x = nhd_long, y = nhd_lat, 
#                    color = factor(levmax_determ_noise))) +
#                scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction"))  + 
#                 pt.size.paper + 
#                 states.lines + 
#                 coord_fixed(1.3) +
#                 th.paper
# lmdn

# ggsave("tp_bystate.pdf")


## cutoff: 95%

nft <- ggplot(gg_dat, 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nf_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nft

# ggsave("tp_bystate.pdf")

# nfd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, 
#                                    color = factor(nf_determ))) +
#   scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction")) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nfd

# ggsave("figures/nfdeterm_extrap.pdf", 
# # width = 860, height = 573,
# units = "mm")

# nfdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nf_determ_noise))) +
#   scale_colour_manual(name = "Extraplation",
#                       values = c("#E41A1C","#A6CEE3")) +  
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nfdn

# ggsave("tp_bystate.pdf")

## cutoff: 99%

nnt <- ggplot(gg_dat, 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nn_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  + 
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nnt

# ggsave("tp_bystate.pdf")

# nnd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat,
#                                    color = factor(nn_determ))) +
#   scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction")) +
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
rm(gg_dat)



# ggarrange(mt, lmt, nnt, nft,  
#           labels = c("A. Maximum Cutoff", 
#                      "B. Leverage Cutoff", 
#                      "C. 99% Cutoff", 
#                      "D. 95% Cutoff"),
#           ncol = 2, nrow = 2, 
#           common.legend = TRUE, 
#           legend = "bottom")
# 
# ggsave("./figures/extrap_TNonly_trace.eps", plot = last_plot(), device = "eps", path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE)


ggarrange(md, lmd, nnd, nfd,  
          labels = c("A. Maximum Cutoff", 
                     "B. Leverage Cutoff", 
                     "C. 99% Cutoff", 
                     "D. 95% Cutoff"),
          ncol = 2, nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom")

ggsave("./figures/extraplocation_TNonly.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)